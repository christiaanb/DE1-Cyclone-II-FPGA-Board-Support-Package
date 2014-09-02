{-# LANGUAGE RecordWildCards #-}
module I2CController.I2CByteController
where

import CLaSH.Prelude hiding (shift)
import Control.Lens
import Control.Monad.State

import DE1Types
import I2CController.I2CBitController
import I2CController.I2CTypes
import Utils

data ByteStateMachine = BYTEidle | BYTEstart | BYTEread | BYTEwrite | BYTEack | BYTEstop
  deriving Show

data MachineState
  = MachineState
  { _cState   :: ByteStateMachine -- State machine
  , _coreCmd  :: I2CCommand       -- coreCmd register
  , _coreTxd  :: Bit              -- coreTxd register
  , _shift    :: Bool             -- shift sr
  , _ld       :: Bool             -- load values in to sr
  , _hostAck  :: Bool             -- host cmd acknowlegde register
  , _ackOut   :: Bool              -- slave ack register
  }

-- i2cMasterByteCtrl clkCnt startCmd stopCmd readCmd writeCmd ackIn dIn i2cIn
--   = (hostAck,ackOut,i2cBusy,i2cAl,dout,i2cOut)
--   where
--     ((coreAck,i2cAl,i2cBusy,coreRxd),i2cOut) = i2cMasterBitCtrl clkCnt (coreCmd,coreTxd) i2cIn

--     -- generate go-signal
--     go = (readCmd ||$ writeCmd ||$ stopCmd) &&$ not1 hostAck

--     -- assign dout output ot shift-register
--     dout = sr

--     -- generate shift register
--     sr = (shiftRegisterT <^> 0) (ld,shift,dIn,coreRxd)

--     -- generate data counter
--     dcnt = (dataCounterT <^> (0 :: Index 8)) (ld,shift)



-- shiftRegisterT sr (ld,shift,dIn,coreRxd) = (sr',sr)
--   where
--     sr' | ld        = dIn
--         | shift     = sr <<# coreRxd
--         | otherwise = sr


-- dataCounterT dcnt (ld,shift) = (dcnt',dcnt)
--   where
--     dcnt' | ld        = 7
--           | shift     = dcnt - 1
--           | otherwise = dcnt

startState :: MachineState
startState = MachineState
           { _cState    = BYTEidle
           , _coreCmd   = I2Cnop
           , _coreTxd   = 0
           , _shift     = False
           , _ld        = False
           , _hostAck   = False
           , _ackOut    = False
           }

makeLenses ''MachineState

nextStateDecoderT :: Bool
                  -> BitVector 8
                  -> Bool
                  -> Bool
                  -> Bit
                  -> Bool
                  -> Bool
                  -> Bool
                  -> Bool
                  -> Bool
                  -> Bool
                  -> State MachineState MachineState
nextStateDecoderT al sr coreAck coreRxd ackIn go cntDone startCmd readCmd writeCmd stopCmd
  | al = get
  | otherwise = do
      (MachineState {..}) <- get

      coreTxd .= msb sr
      shift   .= False
      ld      .= False
      hostAck .= False

      case _cState of
        BYTEidle -> when go $ do
                      if startCmd then do
                          cState  .= BYTEstart
                          coreCmd .= I2Cstart
                      else if readCmd then do
                          cState  .= BYTEread
                          coreCmd .= I2Cread
                      else if writeCmd then do
                          cState  .= BYTEwrite
                          coreCmd .= I2Cwrite
                      else do
                          cState  .= BYTEstop
                          coreCmd .= I2Cstop
                      ld .= True
        BYTEstart -> when coreAck $ do
                       if readCmd then do
                          cState  .= BYTEread
                          coreCmd .= I2Cread
                       else do
                          cState  .= BYTEwrite
                          coreCmd .= I2Cwrite
                       ld .= True
        BYTEwrite -> when coreAck $ do
                       if cntDone then do
                          cState  .= BYTEack
                          coreCmd .= I2Cread
                       else do
                          cState  .= BYTEwrite
                          coreCmd .= I2Cwrite
                          shift   .= True
        BYTEread  -> when coreAck $ do
                       if cntDone then do
                          cState  .= BYTEack
                          coreCmd .= I2Cwrite
                       else do
                          cState  .= BYTEread
                          coreCmd .= I2Cread

                       shift   .= True
                       coreTxd .= ackIn
        BYTEack -> if coreAck then do
                      -- check for stop; Should a STOP command be generated?
                      if stopCmd then do
                         cState  .= BYTEstop
                         coreCmd .= I2Cstop
                      else do
                         cState  .= BYTEidle
                         coreCmd .= I2Cnop

                         -- generate command acknowledge signal
                         hostAck .= True

                      -- assign ackOut output to coreRxd (contains last received bit)
                      ackOut  .= coreRxd
                      coreTxd .= 1
                   else
                      coreTxd .= ackIn
        BYTEstop -> when coreAck $ do
                      cState  .= BYTEidle
                      coreCmd .= I2Cnop

                      -- generate command acknowledge signal
                      hostAck .= True

      get

nextStateDecoder al sr coreAck coreRxd ackIn go cntDone startCmd readCmd writeCmd stopCmd = o
  where
    nextStateDecoderW st al' sr' coreAck' coreRxd' ackIn' go' cntDone' startCmd' readCmd' writeCmd' stopCmd'
      = runState (nextStateDecoderT al' sr' coreAck' coreRxd' ackIn' go' cntDone' startCmd' readCmd' writeCmd' stopCmd') st

    s      = register startState s'
    (o,s') = sWrap (nextStateDecoderW <$> s <*> al <*> sr <*> coreAck <*> coreRxd <*> ackIn <*> go <*> cntDone <*> startCmd <*> readCmd <*> writeCmd <*> stopCmd)

topEntity = nextStateDecoder

-- nextStateDecoder (MachineState {..}) al sr coreAck go cndDone startCmd readCmd writeCmd stopCmd
--     | al = startState
--     | otherwise = case byteStateM of
--         BYTEidle -> if go then
--             if startCmd then
--               MachineState {byteStateM = BYTEstart, icoreCmd = I2Cstart, ..}
--             else
--               sdefault
--           else
--             sdefault
      --     else if readCmd then
      --       sd {byteStateM = BYTEread , coreCmd = I2Cread }
      --     else if writeCmd then
      --       sd {byteStateM = BYTEwrite, coreCmd = I2Cwrite}
      --     else -- stop
      --       sd {byteStateM = BYTEstop , coreCmd = I2Cstop}
      --   else
      --     sd
      -- BYTEstart -> if coreAck then
      --     if readCmd then
      --       sd {byteStateM = BYTEread , coreCmd = I2Cread }
      --     else
      --       sd {byteStateM = BYTEwrite, coreCmd = I2Cwrite}
      --   else
      --     sd
      -- BYTEwrite -> if coreAck then
      --     if cntDone then
      --       sd {byteStateM = BYTEack, coreCmd = I2Cread}
      --     else
      --       sd { byteStateM = BYTEwrite -- stay in same state
      --          , coreCmd    = I2Cwrite  -- write next bit
      --          , shiftsr    = True
      --          }
      --   else
      --     sd
      -- BYTEread -> if coreAck then
      --     if cntDone then
      --       sd {byteStateM = BYTEack, coreCmd = I2Cwrite}
      --     else
      --       sd { byteStateM = BYTEread -- stay in same state
      --            , coreCmd    = I2Cread  -- write next bit
      --            }
      --   else
      --     sd
      -- BYTEack -> if coreAck then
      --   -- check for stop; Should a STOP command be generated?
      --     if stopCmd then
      --       sdat {byteStateM = BYTEstop, coreCmd = I2Cstop}
      --     else
      --       sdat { byteStateM = BYTEidle
      --            , coreCmd    = I2Cnop
      --            , hostAck    = True -- generate command acknowledge signal
      --            }
      --   else
      --     sdt
      -- BYTEstop -> if coreAck then
      --     sd { byteStateM = BYTEidle
      --        , coreCmd    = I2Cstop
      --        , hostAck    = True -- generate command acknowledge signal
      --        }
      --   else
      --     sd
  -- where
  --   sdefault = MachineState
  --            { icoreTxd   = msb sr
  --            , ishift     = False
  --            , ild        = False
  --            , ihostAck   = False
  --            , ..
  --            }


-- i2cMasterByteCtrlInit :: ByteCtrlS
-- i2cMasterByteCtrlInit = ByteS { byteStateM = BYTEidle
--                               , coreCmd    = I2Cnop
--                               , coreTxd    = low
--                               , shiftsr    = False
--                               , ld         = False
--                               , hostAck    = False
--                               , ackOut     = high
--                               , sr         = repeat low
--                               , dcnt       = 0
--                               }

-- i2cMasterByteCtrlT :: ByteCtrlS -> ByteCtrlI -> (ByteCtrlS, ByteCtrlO)
-- i2cMasterByteCtrlT s@(ByteS {..}) inp = (s', outp)
--   where
--     -- ==========
--     -- = Inputs =
--     -- ==========
--     -- start  : generate start command
--     -- stop   : generate stop command
--     -- read   : generate read sequence
--     -- write  : generate write sequence
--     -- ackIn  : send (n)ack to slave
--     -- din    : value for write sequence
--     -- bitResp: output from master bit controller
--     (start,stop,read,write,ackIn,din,bitResp) = inp

--     -- ===============================
--     -- = Signals from bit controller =
--     -- ===============================
--     -- coreAck: acknowledge command
--     -- al     : arbitration lost
--     -- coreRxd: received bit
--     (coreAck,al,coreRxd) = bitResp

--     -- ===========
--     -- = Outputs =
--     -- ===========
--     -- hostAck: acknowledge command to host
--     -- ackOut : ack recevied from slave
--     -- dout   : byte received from slave
--     -- bitCtrl: input for master bit controller
--     outp = (hostAck,ackOut,dout,bitCtrl)

--     -- ==============================
--     -- = Signals for bit controller =
--     -- ==============================
--     -- coreCmd: command to executre
--     -- coreTxd: bit to send
--     bitCtrl = (coreCmd,coreTxd)

--     -- update registers
--     s' = stateMachine { sr = sr', dcnt = dcnt' }

--     -- generate go-signal
--     go = (read || write || stop) && (not hostAck)

--     -- assign dOut the output of the shift-register
--     dout = sr

--     -- generate shift register
--     sr' | rst       = repeat low
--         | ld        = unpack din
--         | shiftsr   = coreRxd +>> sr
--         | otherwise = sr

--     dcnt' | rst       = 0
--           | ld        = 7
--           | shiftsr   = dcnt - 1
--           | otherwise = dcnt

--     cntDone = dcnt == 0

--     -- state machine
--     sd   = s  {coreTxd = last sr, shiftsr = False, ld = False, hostAck = False}
--     sdl  = sd {ld = True}
--     sdst = sd {shiftsr = True, coreTxd = ackIn}
--     sdat = sd { ackOut  = coreRxd -- assign ackOut output to coreRxd (contains last received bit)
--               , coreTxd = high
--               }
--     sdt  = sd {coreTxd = ackIn}

--     stateMachine = if rst || al then
--         s {coreCmd = I2Cnop, coreTxd = low, shiftsr = False, ld = False, hostAck = False, byteStateM = BYTEidle, ackOut = high}
--       else
--         case byteStateM of
--           BYTEidle -> if go then
--               if start then
--                 sdl {byteStateM = BYTEstart, coreCmd = I2Cstart}
--               else if read then
--                 sdl {byteStateM = BYTEread , coreCmd = I2Cread }
--               else if write then
--                 sdl {byteStateM = BYTEwrite, coreCmd = I2Cwrite}
--               else -- stop
--                 sdl {byteStateM = BYTEstop , coreCmd = I2Cstop}
--             else
--               sd

--           BYTEstart -> if coreAck then
--               if read then
--                 sdl {byteStateM = BYTEread , coreCmd = I2Cread }
--               else
--                 sdl {byteStateM = BYTEwrite, coreCmd = I2Cwrite}
--             else
--               sd

--           BYTEwrite -> if coreAck then
--               if cntDone then
--                 sd {byteStateM = BYTEack, coreCmd = I2Cread}
--               else
--                 sd { byteStateM = BYTEwrite -- stay in same state
--                    , coreCmd    = I2Cwrite  -- write next bit
--                    , shiftsr    = True
--                    }
--             else
--               sd

--           BYTEread -> if coreAck then
--               if cntDone then
--                 sdst {byteStateM = BYTEack, coreCmd = I2Cwrite}
--               else
--                 sdst { byteStateM = BYTEread -- stay in same state
--                      , coreCmd    = I2Cread  -- write next bit
--                      }
--             else
--               sd

--           BYTEack -> if coreAck then
--             -- check for stop; Should a STOP command be generated?
--               if stop then
--                 sdat {byteStateM = BYTEstop, coreCmd = I2Cstop}
--               else
--                 sdat { byteStateM = BYTEidle
--                      , coreCmd    = I2Cnop
--                      , hostAck    = True -- generate command acknowledge signal
--                      }
--             else
--               sdt

--           BYTEstop -> if coreAck then
--               sd { byteStateM = BYTEidle
--                  , coreCmd    = I2Cstop
--                  , hostAck    = True -- generate command acknowledge signal
--                  }
--             else
--               sd

