{-# LANGUAGE RecordWildCards #-}
module MasterBit where

import CLaSH.Prelude
import Control.Lens
import Control.Lens.Zoom
import Control.Monad.State

import I2C.Types
import Utils

data BusStatusCtrl
  = BusStatusCtrl
  { _cSCL, _cSDA    :: BitVector 2 -- capture SDA and SCL
  , _filterCnt      :: Unsigned 14 -- clock divider for filter
  , _fSCL, _fSDA    :: BitVector 2 -- filter inputs for SCL and SDA
  , _startCondition :: Bool        -- start detected
  , _stopCondition  :: Bool        -- stop detected
  , _ibusy          :: Bool        -- internal busy signal
  , _cmdStop        :: Bool        -- STOP command
  , _ial            :: Bool        -- internal arbitration lost signal
  }

makeLenses ''BusStatusCtrl

data BitStateMachine
  = Idle |
    StartA | StartB | StartC | StartD | StartE |
    StopA | StopB | StopC | StopD |
    ReadA | ReadB | ReadC | ReadD |
    WriteA | WriteB | WriteC | WriteD
  deriving Eq

data MasterBitS
  = MasterBitS
  { _isclOen, _isdaOen   :: Bool        -- Internal I2C lines
  , _sdaChk              :: Bool        -- check SDA status (multi-master arbiter)
  , _cmdAck              :: Bool        -- command completed
  , _dsclOen             :: Bool        -- delayed sclOen signal
  , _sI2C                :: I2CIPair    -- synchronised SCL and SDA
  , _dI2C                :: I2CIPair    -- delayed version of sI2C
  , _clkEn               :: Bool        -- statemachine clock enable
  , _slaveWait           :: Bool        -- clock generation signals
  , _cnt                 :: Unsigned 16 -- clock devider counter
  , _dout                :: Bit
  , _busState            :: BusStatusCtrl
  , _cState              :: BitStateMachine
  }

makeLenses ''MasterBitS

--startState :: MasterBitS
--startState = MasterBitS
--           { _dscl_oen   = False
--           , _iscl_oen   = True
--           , _slave_wait = False
--           , _dSCL       =
--           }
topEntity = busStatusCtrl

masterBitCtrlT :: MasterBitS
               -> Unsigned 16
               -> I2Ccommand
               -> Bit
               -> I2CIPair
               -> (MasterBitS, MasterBitS)
masterBitCtrlT s clkCnt cmd dIn i2ci = (`runState` s) $ do
  (MasterBitS {..}) <- get

  -- whenever the slave is not ready it can delay the cycle by pulling SCL low
  -- delay scl_oen
  dsclOen .= _isclOen

  -- slave_wait is asserted when master wants to drive SCL high, but the slave pulls it low
  -- slave_wait remains asserted until the slave releases SCL
  let masterSclHigh = _isclOen && not _dsclOen
  slaveWait .= ((masterSclHigh || _slaveWait) && _scli _sI2C == 0)

  -- master drives scl high, but another master pulls it low
  -- master start counting down its low cycle now (clock synchronisation)
  let sclSync = _scli _dI2C == 1 &&
                _scli _sI2C == 0 &&
                _isclOen

  -- generate clk enable signal
  if sclSync then do
     cnt   .= clkCnt
     clkEn .= True
  else if _slaveWait then do
     clkEn .= False
  else do
     cnt   -= 1
     clkEn .= False

  -- generate bus status controller
  -- busStatusCtrl clkCnt cmd i2ci

  get

busStartState
  = BusStatusCtrl
  { _cSCL = 0, _cSDA = 0
  , _filterCnt = 0
  , _fSCL = maxBound, _fSDA = maxBound
  , _startCondition = False
  , _stopCondition = False
  , _ibusy = False
  , _cmdStop = False
  , _ial = False
  }

busStatusCtrl :: Unsigned 16
              -> I2Ccommand
              -> I2CIPair
              -> State MasterBitS ()
busStatusCtrl clkCnt cmd (I2CIPair sclI sdaI) = do
  (MasterBitS {..}) <- get
  let (BusStatusCtrl {..}) = _busState

  -- zoom busState $ do
    -- capture SCL and SDA
  busState.cSCL .= lsb _cSCL ++# sclI
  busState.cSDA .= lsb _cSDA ++# sdaI

  -- filter SCL and SDA; (attempt to) remove glitches
  if _filterCnt == 0 then do
     busState.filterCnt .= unpack (fst (split clkCnt))
  else do
     busState.filterCnt -= 1

  when (_filterCnt == 0) $ do
    busState.fSCL %= (<<# msb _cSCL)
    busState.fSDA %= (<<# msb _cSDA)

  -- generated filtered SCL and SDA signals
  (sI2C.scli) .= ((_fSCL!2 .&. _fSCL!1) .|.
                  (_fSCL!2 .&. _fSCL!0) .|.
                  (_fSCL!1 .&. _fSCL!0))

  (sI2C.sdai) .= ((_fSDA!2 .&. _fSDA!1) .|.
                  (_fSDA!2 .&. _fSDA!0) .|.
                  (_fSDA!1 .&. _fSDA!0))

  -- Delayed version of sI2C
  dI2C .= _sI2C

  let sSDA = _sdai _sI2C
      dSDA = _sdai _dI2C
      sSCL = _scli _sI2C
      dSCL = _scli _dI2C

  -- zoom busState $ do
  -- detect start condition => detect falling edge on SDA while SCL is high
  -- detect stop condition => detect rising edge on SDA while SCL is high
  busState.startCondition .= ((sSDA == 0 && dSDA == 1) && (sSCL == 1))
  busState.stopCondition  .= ((sSDA == 1 && dSDA == 0) && (sSCL == 1))

  -- generate i2c-bus busy signal
  busState.ibusy .= ((_startCondition || _ibusy) && not _stopCondition)

  -- generate arbitration lost signal
  -- arbitration lost when:
  -- 1) master drives SDA high, but the i2c bus is low
  -- 2) stop detected while not requested (detect during 'idle' state)
  when _clkEn (busState.cmdStop .= (cmd == I2C_Stop))

  let masterHighBusLow = _sdaChk && sSDA == 0 && _isdaOen
  if _cState == Idle then
     busState.ial .= (masterHighBusLow || (_stopCondition && not _cmdStop))
  else
     busState.ial .= masterHighBusLow

  -- generate dout signal, store out on rising edge of SCL
  when (sSCL == 1 && dSCL == 0) (dout .= sSDA)

nextStateDecoder cmd = do
  (MasterBitS {..}) <- get

  -- default no acknowledge
  cmdAck .= False

  when _clkEn $ do
    case _cState of
      Idle -> do
        case cmd of
          I2C_Start -> cState .= StartA
          I2C_Stop  -> cState .= StopA
          I2C_Write -> cState .= WriteA
          I2C_Read  -> cState .= ReadA
          _         -> cState .= Idle -- NOP command

        sdaChk .= False

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

