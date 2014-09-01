{-# LANGUAGE RecordWildCards #-}
module Keyboard.ConstantKey where

import CLaSH.Prelude
import DE1Types
import Utils

data KeyState = KeyUp | KeyDown | KeyIdle
  deriving (Show)

data ConstantKeyS = CK { stateM    :: KeyState
                       , key       :: Scancode
                       , byteReadP :: Bool
                       }


type ConstantKeyI = (Scancode, Bool)
type ConstantKeyO = (SegDisp, SegDisp, Scancode)

constantKey :: SWrapped ConstantKeyI
            -> SWrapped ConstantKeyO
constantKey = constantKeyT <^> constantKeyInit

constantKeyInit :: ConstantKeyS
constantKeyInit = CK KeyIdle 0x0 False

constantKeyT :: ConstantKeyS -> ConstantKeyI -> (ConstantKeyS, ConstantKeyO)
constantKeyT s@(CK{..}) (scancode,byteRead) = (s', (dig1,dig0,key))
  where
    sd = s {byteReadP = byteRead}

    newScancode = byteRead && (not byteReadP)

    s' = case stateM of
      KeyIdle -> if newScancode then
          sd {stateM = KeyDown, key = scancode}
        else
          sd {key = 0x0}
      KeyDown -> if newScancode && scancode == 0xF0 then
          sd {stateM = KeyUp, key = 0x0}
        else
          sd
      KeyUp -> if newScancode then
          sd {stateM = KeyIdle}
        else
          sd

    dig1 = hex2display (fst (split key))
    dig0 = hex2display (snd (split key))
