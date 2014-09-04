{-# LANGUAGE RecordWildCards #-}
module I2C.MasterBit
  (masterBitCtrl
  )
where

import CLaSH.Prelude
import Control.Lens
import Control.Lens.Zoom
import Control.Monad.State

import I2C.Types
import Utils

data BusStatusCtrl
  = BusStatusCtrl
  { _cI2C           :: Vec 2 I2CIPair -- capture SDA and SCL
  , _filterCnt      :: Unsigned 14    -- clock divider for filter
  , _fI2C           :: Vec 3 I2CIPair -- filtered inputs for SCL and SDA
  , _sI2C           :: I2CIPair       -- synchronised SCL and SDA
  , _dI2C           :: I2CIPair       -- delayed version of 'sI2C'
  , _startCondition :: Bool           -- start detected
  , _stopCondition  :: Bool           -- stop detected
  , _ibusy          :: Bool           -- internal busy signal
  , _cmdStop        :: Bool           -- STOP command
  , _ial            :: Bool           -- internal arbitration lost signal
  }

makeLenses ''BusStatusCtrl

data BitStateMachine
  = Idle |
    StartA | StartB | StartC | StartD | StartE |
    StopA | StopB | StopC | StopD |
    ReadA | ReadB | ReadC | ReadD |
    WriteA | WriteB | WriteC | WriteD
  deriving Eq

data StateMachine
  = StateMachine
  { _isclOen, _isdaOen   :: Bool        -- Internal I2C lines
  , _sdaChk              :: Bool        -- check SDA status (multi-master arbiter)
  , _cmdAck              :: Bool        -- command completed
  , _cState              :: BitStateMachine
  }

makeLenses ''StateMachine

data MasterBitS
  = MasterBitS
  { _dsclOen             :: Bool        -- delayed sclOen signal
  , _slaveWait           :: Bool        -- clock generation signals
  , _cnt                 :: Unsigned 16 -- clock devider counter
  , _clkEn               :: Bool        -- statemachine clock enable
  , _dout                :: Bit
  , _busState            :: BusStatusCtrl
  , _stateMachine        :: StateMachine
  }

makeLenses ''MasterBitS

masterBitCtrl :: Unsigned 16
              -> Signal I2Ccommand
              -> Signal Bit
              -> Signal I2CIPair
              -> (Signal (Bool,Bool,Bool,Bit), Signal I2COPair)
masterBitCtrl clkCnt cmd dIn i2ci = sWrap o
  where
    s      = register masterStartState s'
    (o,s') = sWrap (masterBitCtrlT clkCnt <$> cmd <*> dIn <*> i2ci <*> s)


masterStartState :: MasterBitS
masterStartState
  = MasterBitS
  { _dsclOen      = False
  , _slaveWait    = False
  , _cnt          = 0
  , _clkEn        = True
  , _dout         = 0
  , _busState     = busStartState
  , _stateMachine = stateMachineStart
  }


masterBitCtrlT :: Unsigned 16
               -> I2Ccommand
               -> Bit
               -> I2CIPair
               -> MasterBitS
               -> ( ((Bool,Bool,Bool,Bit),I2COPair)
                  , MasterBitS)
masterBitCtrlT clkCnt cmd dIn i2ci = runState $ do
  (MasterBitS {..}) <- get
  -- Extract substate information
  let sSCL      = _scli (_sI2C _busState)
      dSCL      = _scli (_dI2C _busState)
      sSDA      = _sdai (_sI2C _busState)
      al        = _ial _busState
      busy      = _ibusy _busState
      (StateMachine isclOen isdaOen sdaChk cmdAck cState) = _stateMachine

  -- whenever the slave is not ready it can delay the cycle by pulling SCL low
  -- delay scl_oen
  dsclOen .= isclOen

  -- slave_wait is asserted when master wants to drive SCL high, but the slave pulls it low
  -- slave_wait remains asserted until the slave releases SCL
  let masterSclHigh = isclOen && not _dsclOen
  slaveWait .= ((masterSclHigh || _slaveWait) && sSCL == 0)

  -- master drives scl high, but another master pulls it low
  -- master start counting down its low cycle now (clock synchronisation)
  let sclSync = sSCL == 1 &&
                dSCL == 0 &&
                isclOen

  -- generate clk enable signal
  if sclSync then do
     cnt .= clkCnt
     clkEn .= True
  else if _slaveWait then do
     clkEn .= False
  else do
     cnt -= 1
     clkEn .= False

  -- generate bus status controller
  zoom busState (busStatusCtrl clkCnt cmd sdaChk isdaOen _clkEn cState i2ci)

  -- generate dout signal, store out on rising edge of SCL
  when (sSCL == 1 && dSCL == 0) (dout .= sSDA)

  -- generate statemachine
  zoom stateMachine (nextStateDecoder cmd _clkEn dIn)

  -- assign outputs
  return ( (cmdAck,al,busy,_dout)
         , I2COPair 0 isclOen 0 isdaOen
         )

busStartState
  = BusStatusCtrl
  { _cI2C           = repeat (I2CIPair 0 0)
  , _filterCnt      = 0
  , _fI2C           = repeat (I2CIPair 1 1)
  , _sI2C           = (I2CIPair 1 1)
  , _dI2C           = (I2CIPair 1 1)
  , _startCondition = False
  , _stopCondition  = False
  , _ibusy          = False
  , _cmdStop        = False
  , _ial            = False
  }

{-# NOINLINE busStatusCtrl #-}
busStatusCtrl :: Unsigned 16
              -> I2Ccommand
              -> Bool
              -> Bool
              -> Bool
              -> BitStateMachine
              -> I2CIPair
              -> State BusStatusCtrl ()
busStatusCtrl clkCnt cmd sdaChk isdaOen clkEn cState i2ci  = do
    (BusStatusCtrl {..}) <- get

    -- capture SCL and SDA
    cI2C .= (i2ci +>> _cI2C)

    -- filter SCL and SDA (attempt to) remove glitches
    if _filterCnt == 0 then do
       filterCnt .= unpack (fst (split clkCnt))
    else do
       filterCnt -= 1

    when (_filterCnt == 0) $
      fI2C .= (last _cI2C +>> _fI2C)

    -- generated filtered SCL and SDA signals
    (sI2C.scli) .= filterT (map _scli _fI2C)
    (sI2C.sdai) .= filterT (map _sdai _fI2C)

    -- Delayed version of sI2C
    dI2C .= _sI2C

    let sSDA = _sdai _sI2C
        dSDA = _sdai _dI2C
        sSCL = _scli _sI2C

    -- zoom busState $ do
    -- detect start condition -> do detect falling edge on SDA while SCL is high
    -- detect stop condition -> do detect rising edge on SDA while SCL is high
    startCondition .= ((sSDA == 0 && dSDA == 1) && (sSCL == 1))
    stopCondition  .= ((sSDA == 1 && dSDA == 0) && (sSCL == 1))

    -- generate i2c-bus busy signal
    ibusy .= ((_startCondition || _ibusy) && not _stopCondition)

    -- generate arbitration lost signal
    -- arbitration lost when:
    -- 1) master drives SDA high, but the i2c bus is low
    -- 2) stop detected while not requested (detect during 'idle' state)
    when clkEn (cmdStop .= (cmd == I2C_Stop))

    let masterHighBusLow = sdaChk && sSDA == 0 && isdaOen
    if cState == Idle then
       ial .= (masterHighBusLow || (_stopCondition && not _cmdStop))
    else
       ial .= masterHighBusLow
  where
    filterT f = (f!!2 .&. f!!1) .|.
                (f!!2 .&. f!!0) .|.
                (f!!1 .&. f!!0)

stateMachineStart
  = StateMachine
  { _isclOen = True
  , _isdaOen = True
  , _sdaChk  = False
  , _cmdAck  = False
  , _cState  = Idle
  }

-- topEntity a b c s = runState (nextStateDecoder a b c) s

{-# NOINLINE nextStateDecoder #-}
nextStateDecoder :: I2Ccommand
                 -> Bool
                 -> Bit
                 -> State StateMachine ()
nextStateDecoder cmd clkEn dIn = do
  (StateMachine {..}) <- get

  -- default no acknowledge
  cmdAck .= False

  when clkEn $ do
    case _cState of
      -- idle
      Idle -> do
          case cmd of
            I2C_Start -> cState .= StartA
            I2C_Stop  -> cState .= StopA
            I2C_Write -> cState .= WriteA
            I2C_Read  -> cState .= ReadA
            _         -> cState .= Idle -- NOP command

          sdaChk .= False

      -- start
      StartA -> do
          cState  .= StartB
          isdaOen .= True  -- set SDA high
          sdaChk  .= False -- don't check SDA

      StartB -> do
          cState  .= StartC
          isclOen .= True  -- set SCL high
          isdaOen .= True  -- keep SDA high
          sdaChk  .= False -- don't check SDA

      StartC -> do
          cState  .= StartD
          isclOen .= True  -- keep SCL high
          isdaOen .= False -- set SDA low
          sdaChk  .= False -- don't check SDA

      StartD -> do
          cState  .= StartE
          isclOen .= True  -- keep SCL high
          isdaOen .= False -- keep SDA low
          sdaChk  .= False -- don't check SDA

      StartE -> do
          cState  .= Idle
          cmdAck  .= True  -- command completed
          isclOen .= False -- set SCL low
          sdaChk  .= False -- don't check SDA

      -- stop
      StopA -> do
          cState  .= StopB
          isclOen .= False -- keep SCL low
          isdaOen .= False -- set SDA low
          sdaChk  .= False -- don't check SDA

      StopB -> do
          cState  .= StopC
          isclOen .= True -- set SCL high
          isdaOen .= False -- keep SDA low
          sdaChk  .= False -- don't check SDA

      StopC -> do
          cState  .= StopD
          isclOen .= True -- keep SCL high
          isdaOen .= False -- keep SDA low
          sdaChk  .= False -- don't check SDA

      StopD -> do
          cState  .= Idle
          cmdAck .= True -- command completed
          isclOen .= True -- keep SCL high
          isdaOen .= True -- set SDA high
          sdaChk  .= False -- don't check SDA

      -- read
      ReadA -> do
          cState  .= ReadB
          isclOen .= False -- keep SCL low
          isdaOen .= True -- tri-state SDA
          sdaChk  .= False -- don't check SDA

      ReadB -> do
          cState  .= ReadC
          isclOen .= True -- set SCL high
          isdaOen .= True -- tri-state SDA
          sdaChk  .= False -- don't check SDA

      ReadC -> do
          cState  .= ReadD
          isclOen .= True -- keep SCL high
          isdaOen .= True -- tri-state SDA
          sdaChk  .= False -- don't check SDA

      ReadD -> do
          cState  .= Idle
          cmdAck  .= True -- command completed
          isclOen .= False -- set SCL low
          isdaOen .= True -- tri-state SDA
          sdaChk  .= False -- don't check SDA

      -- write
      WriteA -> do
          cState  .= WriteB
          isclOen .= False -- keep SCL low
          isdaOen .= (dIn == 1) -- set SDA
          sdaChk  .= False -- don't check SDA (SCL low)

      WriteB -> do
          cState  .= WriteC
          isclOen .= True -- set SCL high
          isdaOen .= (dIn == 1) -- keep SDA
          sdaChk  .= False -- don't check SDA yet
                           -- Allow some more time for SDA and SCL to settle

      WriteC -> do
          cState  .= WriteD
          isclOen .= True -- keep SCL high
          isdaOen .= (dIn == 1) -- keep SDA
          sdaChk  .= True -- check SDA

      WriteD -> do
          cState  .= Idle
          cmdAck  .= True -- command completed
          isclOen .= False -- set SCL low
          isdaOen .= (dIn == 1) -- keep SDA
          sdaChk  .= False -- don't check SDA (SCL low)
