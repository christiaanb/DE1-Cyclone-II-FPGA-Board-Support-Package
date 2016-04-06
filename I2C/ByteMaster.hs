{-# LANGUAGE RecordWildCards #-}
module I2C.ByteMaster  where

import CLaSH.Prelude

import Control.Lens hiding (Index)
import Control.Monad
import Control.Monad.Trans.State
import Data.Tuple

import I2C.ByteMaster.ShiftRegister
import I2C.Types

data ByteStateMachine = Idle | Start | Read | Write | Ack | Stop
  deriving Show

data ByteMasterS
  = ByteS
  { _srState    :: ShiftRegister
  , _byteStateM :: ByteStateMachine -- State machine
  , _coreCmd    :: I2CCommand       -- coreCmd register
  , _coreTxd    :: Bit              -- coreTxd register
  , _shiftsr    :: Bool             -- shift sr
  , _ld         :: Bool             -- load values in to sr
  , _hostAck    :: Bool             -- host cmd acknowlegde register
  , _ackOut     :: Bool             -- slave ack register
  }

makeLenses ''ByteMasterS

type ByteMasterI = (Bool,Bool,Bool,Bool,Bool,Bool,BitVector 8,BitRespSig)
type ByteMasterO = (Bool,Bool,BitVector 8,BitCtrlSig)

{-# INLINE byteMasterInit #-}
byteMasterInit :: ByteMasterS
byteMasterInit
  = ByteS
  { _srState    = shiftStartState
  , _byteStateM = Idle
  , _coreCmd    = I2Cnop
  , _coreTxd    = low
  , _shiftsr    = False
  , _ld         = False
  , _hostAck    = False
  , _ackOut     = True
  }

{-# NOINLINE byteMasterT #-}
byteMasterT :: ByteMasterS -> ByteMasterI -> (ByteMasterS, ByteMasterO)
byteMasterT s@(ByteS {_srState = ShiftRegister {..}, ..})
            (rst,start,stop,read,write,ackIn,din,~(coreAck,al,coreRxd)) = swap $ flip runState s $ do
      -- generate go-signal
  let go = (read || write || stop) && (not _hostAck)

      -- assign dOut the output of the shift-register
      dout = _sr

  cntDone <- zoom srState (shiftRegister rst _ld _shiftsr (bv2v din) coreRxd)

  -- state machine
  coreTxd .= head dout
  shiftsr .= False
  ld      .= False
  hostAck .= False

  if rst || al then do
    coreCmd    .= I2Cnop
    coreTxd    .= low
    byteStateM .= Idle
    ackOut     .= True
  else case _byteStateM of
    Idle -> when go $ do
      ld .= True
      if start then do
        byteStateM .= Start
        coreCmd    .= I2Cstart
      else if read then do
        byteStateM .= Read
        coreCmd    .= I2Cread
      else if write then do
        byteStateM .= Write
        coreCmd    .= I2Cwrite
      else do-- stop
        byteStateM .= Stop
        coreCmd    .= I2Cstop
    Start -> when coreAck $ do
      ld .= True
      if read then do
        byteStateM .= Read
        coreCmd    .= I2Cread
      else do
        byteStateM .= Write
        coreCmd    .= I2Cwrite
    Write -> when coreAck $ do
      if cntDone then do
        byteStateM .= Ack
        coreCmd    .= I2Cread
      else do
        coreCmd    .= I2Cwrite
        shiftsr    .= True
    Read -> when coreAck $ do
      shiftsr .= True
      coreTxd .= pack ackIn
      if cntDone then do
        byteStateM .= Ack
        coreCmd    .= I2Cwrite
      else do
        coreCmd    .= I2Cread
    Ack -> if coreAck then do
        ackOut  .= unpack coreRxd
        coreTxd .= high
        -- check for stop; Should a STOP command be generated?
        if stop then do
          byteStateM .= Stop
          coreCmd    .= I2Cstop
        else do
          byteStateM .= Idle
          coreCmd    .= I2Cnop
          -- generate command acknowledge signal
          hostAck    .= True
      else
        coreTxd .= pack ackIn
    Stop -> when coreAck $ do
      byteStateM .= Idle
      coreCmd    .= I2Cnop
      hostAck    .= True

  let bitCtrl = (_coreCmd,_coreTxd)
      outp    = (_hostAck,_ackOut,v2bv dout,bitCtrl)

  return outp
