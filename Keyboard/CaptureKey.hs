module Keyboard.CaptureKey (captureKey) where

import CLaSH.Prelude
import CLaSH.Prelude.Explicit
import DE1Types
import Utils

type CaptureKeyS = (Vec 10 Bit, Index 12, Bit)
type CaptureKeyI = (Bit,Bit)
type CaptureKeyO = (SegDisp,SegDisp,Scancode,Bool)

captureKey :: CSignal KBClock (Bit,Bit)
           -> Wrapped SystemClock CaptureKeyO
captureKey kbInp = captureOut
  where
    kbInpS     = wordSynchronize kbClock systemClock kbdataInit kbInp
    captureOut = (captureKeyT <^> captureKeyInit) (sWrap kbInpS)

kbdataInit :: (Bit,Bit)
kbdataInit = (low,high)

captureKeyInit :: CaptureKeyS
captureKeyInit = (repeat low, 0, high)

captureKeyT :: CaptureKeyS
            -> CaptureKeyI
            -> (CaptureKeyS, CaptureKeyO)
captureKeyT (buffer,iteration,kbclockP) (kbdata,kbclock) =
    ((buffer',iteration',kbclockP'), (dig3,dig2,scancode,byteRead))
  where
    kbclockP' = kbclock

    kbClockFalling = kbclockP == high && kbclock == low

    buffer' = if kbClockFalling then buffer <<+ kbdata else buffer


    iteration' = if kbClockFalling then
        if iteration /= 11 then iteration + 1 else 1
      else
        iteration

    scancode = pack (take d8 buffer)
    byteRead = iteration == 11

    dig3     = hex2display (fst (split scancode))
    dig2     = hex2display (snd (split scancode))
