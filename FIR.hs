{-# LANGUAGE TemplateHaskell, Arrows #-}
module FIR where

import CLasH.HardwareTypes
import DE1Types
import Utils

type Word = Signed D18
type DWord = Signed D36

fpmult :: Word -> Word -> Word
fpmult i h = o
  where vi = (resizeSigned i) :: DWord
        vh = (resizeSigned h) :: DWord
        o = (resizeSigned (shiftR (vi * vh) 17)) :: Word


-- cutoff freq = 938 Hz
lowpass :: Vector D17 Word
lowpass = $(vTH [ 1046, 1565, 3024, 5274, 8007, 10808, 13227, 14863, 15442, 14863, 13227, 10808, 8007,  5274,  3024,  1565,  (1046 :: Word) ] )

highpass :: Vector D17 Word
highpass = $(vTH [ 130025, -1565, -3024, -5274, -8007, -10808, -13227, -14863, -15442, -14863, -13227, -10808, -8007,  -5274,  -3024,  -1565,  (-1046 :: Word) ] )


fir17T :: State (Vector D17 Word) -> (Word, Bool, Vector D17 Word) -> (State (Vector D17 Word), Word)
fir17T (State us) (x,enable,hs) = (State usn, y)
  where 
    ms = vmap (fpmult x) hs
    usn' = vzipWith (+) (us <<+ 0) ms
    usn = if enable then usn' else us
    y = vhead us

-- input range is 2^17-1 .. -2^17
fir17filter = comp fir17T (vcopy 0) fftClock

fir17 :: Comp (Word,Bit,Bool) (Word,Bit)
fir17 = proc (audioData,syncPulse,highorlow) -> do
  firEnable    <- comp pulseHigh Low fftClock -< syncPulse
  filteredData <- fir17filter                 -< (audioData, firEnable, if highorlow then highpass else lowpass)
  syncPulseD   <- comp delayN (singleton Low) fftClock -< syncPulse
  returnA -< (filteredData,syncPulseD)

fir17sync :: Comp (Word, Bit, Bool, Bool) (Word, Bit)
fir17sync = proc (lAdcData,pulseAdc48KHz,enable,highorlow) -> do
  (lAdcDataS, pulseAdc48KHzS) <- comp synchronize firAudDataInit fftClock -< (lAdcData, pulseAdc48KHz)
  filtered                    <- fir17                                    -< (lAdcDataS, pulseAdc48KHzS,highorlow)
  returnA  -< (if enable then filtered else (lAdcData,pulseAdc48KHz))

firAudDataInit :: Vector D2 (Word, Bit)
firAudDataInit = vcopy (0,Low)

-- Test the filter
res = simulate fir17 firdata

firdata  = zip3 (concatMap datavals firdata') enables (repeat True)
firdata' = [(-2)^15-1,(2)^15-1,(-2)^15-1,(2)^15-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
datavals  = replicate 5 
enables   = [High,High,Low,Low,Low] ++ enables

