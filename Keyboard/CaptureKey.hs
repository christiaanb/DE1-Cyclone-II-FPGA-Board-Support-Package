{-# LANGUAGE Arrows #-}
module Keyboard.CaptureKey (captureKey) where

import CLasH.HardwareTypes
import DE1Types
import Utils

type CaptureKeyS = (Vector D10 Bit, Index D12, Bit)
type CaptureKeyI = (Bit,Bit)
type CaptureKeyO = (SegDisp,SegDisp,Scancode,Bool)

captureKey = proc (kbdata,kbclock) -> do
  (kbdataS,kbclockS) <- comp synchronize kbdataInit sysclock     -< (kbdata,kbclock)
  outp               <- comp captureKeyT captureKeyInit sysclock -< (kbdataS,kbclockS)
  returnA -< outp

kbdataInit = vcopy (Low,High)

captureKeyInit :: CaptureKeyS
captureKeyInit = (vcopy Low, 0, High)

captureKeyT :: State CaptureKeyS -> CaptureKeyI -> (State CaptureKeyS, CaptureKeyO)
captureKeyT (State (buffer,iteration,kbclockP)) (kbdata,kbclock) = (State (buffer',iteration',kbclockP'), (dig3,dig2,scancode,byteRead))
  where
    kbclockP' = kbclock
    
    kbClockFalling = kbclockP == High && kbclock == Low
    
    buffer' = if kbClockFalling then kbdata +>> buffer else buffer
    

    iteration' = if kbClockFalling then
        if iteration /= 11 then iteration + 1 else 1
      else
        iteration
               
    scancode = vdrop d2 buffer
    byteRead = iteration == 11
    
    dig3     = hex2display (bv2u (vtake d4 scancode))
    dig2     = hex2display (bv2u (vdrop d4 scancode))
