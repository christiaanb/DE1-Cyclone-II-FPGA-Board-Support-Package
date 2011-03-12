{-# LANGUAGE Arrows, RecordWildCards #-}
module Keyboard.ConstantKey where
  
import CLasH.HardwareTypes
import DE1Types
import Utils
import Debug.Trace

data KeyState = KeyUp | KeyDown | KeyIdle
  deriving (Show)

data ConstantKeyS = CK { stateM    :: KeyState
                       , key       :: Key
                       , byteReadP :: Bool
                       }
                       

type ConstantKeyI = (Scancode, Bool)
type ConstantKeyO = (SegDisp, SegDisp, Key)

constantKey = proc (scancode,byteRead) -> do
  -- (scancodeS, byteReadS) <- (comp synchronize newScancodeInit sysclock) -< (scancode,byteRead)
  (dig1,dig0,key)        <- (comp constantKeyT constantKeyInit sysclock) -< (scancode, byteRead)
  returnA -< (dig1,dig0,key)

-- newScancodeInit = vcopy (vcopy Low, False)

constantKeyInit :: ConstantKeyS
constantKeyInit = CK KeyIdle 0x0 False

constantKeyT :: State ConstantKeyS -> ConstantKeyI -> (State ConstantKeyS, ConstantKeyO)
constantKeyT (State s@(CK{..})) inp = (State s', outp)
  where
    (scancode,byteRead) = inp
    outp = (dig1,dig0,key)
    
    sd = s {byteReadP = byteRead}
    
    newScancode = byteRead && (not byteReadP)
    
    s' = case stateM of
      KeyIdle -> if newScancode then
          sd {stateM = KeyDown, key = bv2u scancode}
        else
          sd {key = 0x0}
      KeyDown -> if newScancode && (bv2u scancode) == 0xF0 then
          sd {stateM = KeyUp, key = 0x0}
        else
          sd
      KeyUp -> if newScancode then
          sd {stateM = KeyIdle}
        else
          sd

    dig1 = hex2display (resizeUnsigned (shiftR key 4))
    dig0 = hex2display (resizeUnsigned key)
