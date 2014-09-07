{-# LANGUAGE RecordWildCards #-}
module I2C.MasterByte.ShiftRegister
  ( ShiftRegister(..)
  , shiftStartState
  , shiftRegister
  )
where

import CLaSH.Prelude
import Control.Lens hiding (Index)
import Control.Monad.State

import Utils

data ShiftRegister
  = ShiftRegister
  { _sr   :: BitVector 8
  , _dCnt :: Index 8
  }

makeLenses ''ShiftRegister

shiftStartState
  = ShiftRegister
  { _sr = 0
  , _dCnt = 0
  }

{-# NOINLINE shiftRegister #-}
shiftRegister :: Bool
              -> Bool
              -> BitVector 8
              -> Bit
              -> State ShiftRegister Bool
shiftRegister ld shift dIn coreRxd = do
  (ShiftRegister {..}) <- get

  -- shift register
  if ld then
    sr .= dIn
  else when shift $
    sr .= _sr <<# coreRxd

  -- data-counter
  if ld then
    dCnt .= 7
  else when shift $
    dCnt -= 1

  return (_dCnt == 0)
