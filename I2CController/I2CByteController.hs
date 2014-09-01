{-# LANGUAGE RecordWildCards #-}
module I2CController.I2CByteController  where

import CLaSH.Prelude
import DE1Types
import I2CController.I2CTypes
import Utils

data ByteStateMachine = BYTEidle | BYTEstart | BYTEread | BYTEwrite | BYTEack | BYTEstop
  deriving Show

data ByteCtrlS = ByteS { byteStateM :: ByteStateMachine -- State machine
                       , coreCmd    :: I2CCommand       -- coreCmd register
                       , coreTxd    :: Bit              -- coreTxd register
                       , shiftsr    :: Bool             -- shift sr
                       , ld         :: Bool             -- load values in to sr
                       , hostAck    :: Bool             -- host cmd acknowlegde register
                       , ackOut     :: Bit              -- slave ack register
                       , sr         :: Vec 8 Bit        -- shift register
                       , dcnt       :: Index 8          -- data counter
                       }

type ByteCtrlI = (Bool,Bool,Bool,Bool,Bool,Bit,Vec 8 Bit,BitRespSig)
type ByteCtrlO = (Bool,Bit,Vec 8 Bit, BitCtrlSig)

i2cMasterByteCtrlInit :: ByteCtrlS
i2cMasterByteCtrlInit = ByteS { byteStateM = BYTEidle
                              , coreCmd    = I2Cnop
                              , coreTxd    = low
                              , shiftsr    = False
                              , ld         = False
                              , hostAck    = False
                              , ackOut     = high
                              , sr         = repeat low
                              , dcnt       = 0
                              }

i2cMasterByteCtrlT :: ByteCtrlS -> ByteCtrlI -> (ByteCtrlS, ByteCtrlO)
i2cMasterByteCtrlT s@(ByteS {..}) inp = (s', outp)
  where
    -- ==========
    -- = Inputs =
    -- ==========
    -- rst    : synchronous reset, assert high
    -- start  : generate start command
    -- stop   : generate stop command
    -- read   : generate read sequence
    -- write  : generate write sequence
    -- ackIn  : send (n)ack to slave
    -- din    : value for write sequence
    -- bitResp: output from master bit controller
    (rst,start,stop,read,write,ackIn,din,bitResp) = inp

    -- ===============================
    -- = Signals from bit controller =
    -- ===============================
    -- coreAck: acknowledge command
    -- al     : arbitration lost
    -- coreRxd: received bit
    (coreAck,al,coreRxd) = bitResp

    -- ===========
    -- = Outputs =
    -- ===========
    -- hostAck: acknowledge command to host
    -- ackOut : ack recevied from slave
    -- dout   : byte received from slave
    -- bitCtrl: input for master bit controller
    outp = (hostAck,ackOut,dout,bitCtrl)

    -- ==============================
    -- = Signals for bit controller =
    -- ==============================
    -- coreCmd: command to executre
    -- coreTxd: bit to send
    bitCtrl = (coreCmd,coreTxd)

    -- update registers
    s' = stateMachine { sr = sr', dcnt = dcnt' }

    -- generate go-signal
    go = (read || write || stop) && (not hostAck)

    -- assign dOut the output of the shift-register
    dout = sr

    -- generate shift register
    sr' | rst       = repeat low
        | ld        = din
        | shiftsr   = sr <<+ coreRxd
        | otherwise = sr

    dcnt' | rst       = 0
          | ld        = 7
          | shiftsr   = dcnt - 1
          | otherwise = dcnt

    cntDone = dcnt == 0

    -- state machine
    sd   = s  {coreTxd = head sr, shiftsr = False, ld = False, hostAck = False}
    sdl  = sd {ld = True}
    sdst = sd {shiftsr = True, coreTxd = ackIn}
    sdat = sd { ackOut  = coreRxd -- assign ackOut output to coreRxd (contains last received bit)
              , coreTxd = high
              }
    sdt  = sd {coreTxd = ackIn}

    stateMachine = if rst || al then
        s {coreCmd = I2Cnop, coreTxd = low, shiftsr = False, ld = False, hostAck = False, byteStateM = BYTEidle, ackOut = high}
      else
        case byteStateM of
          BYTEidle -> if go then
              if start then
                sdl {byteStateM = BYTEstart, coreCmd = I2Cstart}
              else if read then
                sdl {byteStateM = BYTEread , coreCmd = I2Cread }
              else if write then
                sdl {byteStateM = BYTEwrite, coreCmd = I2Cwrite}
              else -- stop
                sdl {byteStateM = BYTEstop , coreCmd = I2Cstop}
            else
              sd

          BYTEstart -> if coreAck then
              if read then
                sdl {byteStateM = BYTEread , coreCmd = I2Cread }
              else
                sdl {byteStateM = BYTEwrite, coreCmd = I2Cwrite}
            else
              sd

          BYTEwrite -> if coreAck then
              if cntDone then
                sd {byteStateM = BYTEack, coreCmd = I2Cread}
              else
                sd { byteStateM = BYTEwrite -- stay in same state
                   , coreCmd    = I2Cwrite  -- write next bit
                   , shiftsr    = True
                   }
            else
              sd

          BYTEread -> if coreAck then
              if cntDone then
                sdst {byteStateM = BYTEack, coreCmd = I2Cwrite}
              else
                sdst { byteStateM = BYTEread -- stay in same state
                     , coreCmd    = I2Cread  -- write next bit
                     }
            else
              sd

          BYTEack -> if coreAck then
            -- check for stop; Should a STOP command be generated?
              if stop then
                sdat {byteStateM = BYTEstop, coreCmd = I2Cstop}
              else
                sdat { byteStateM = BYTEidle
                     , coreCmd    = I2Cnop
                     , hostAck    = True -- generate command acknowledge signal
                     }
            else
              sdt

          BYTEstop -> if coreAck then
              sd { byteStateM = BYTEidle
                 , coreCmd    = I2Cstop
                 , hostAck    = True -- generate command acknowledge signal
                 }
            else
              sd

