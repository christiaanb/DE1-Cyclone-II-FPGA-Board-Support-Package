module FIR where

import CLaSH.Prelude
import CLaSH.Prelude.Explicit
import DE1Types
import Utils

fpmult :: FWord -> FWord -> FWord
fpmult i h = o
  where vi = (resize i) :: DWord
        vh = (resize h) :: DWord
        o = (resize (shiftR (vi * vh) 17)) :: FWord


-- cutoff freq = 938 Hz
lowpass :: Vec 17 FWord
lowpass = $(v [ 1046, 1565, 3024, 5274, 8007, 10808, 13227, 14863, 15442, 14863, 13227, 10808, 8007,  5274,  3024,  1565,  (1046 :: Signed 18) ] )

highpass :: Vec 17 FWord
highpass = $(v [ 130025, -1565, -3024, -5274, -8007, -10808, -13227, -14863, -15442, -14863, -13227, -10808, -8007,  -5274,  -3024,  -1565,  (-1046 :: Signed 18) ] )


fir17T :: (Vec 17 FWord) -> (FWord, Bool, Vec 17 FWord) -> ((Vec 17 FWord), FWord)
fir17T (us) (x,enable,hs) = (usn, y)
  where
    ms = map (fpmult x) hs
    usn' = zipWith (+) (us <<+ 0) ms
    usn = if enable then usn' else us
    y = head us

-- input range is 2^17-1 .. -2^17
fir17filter = mealyB' fftClock fir17T (repeat 0)

fir17 :: Signal' FFTClock FWord -> Signal' FFTClock Bit
      -> Signal' FFTClock Bool -> (Signal' FFTClock FWord, Signal' FFTClock Bit)
fir17 audioData syncPulse highorlow = (filteredData,syncPulseD)
  where
    firEnable    = pulseHigh fftClock syncPulse
    filteredData = fir17filter (audioData, firEnable, mux highorlow (pure highpass) (pure lowpass))
    syncPulseD   = delayN fftClock (singleton 0) syncPulse

fir17sync :: Signal' BClkClock FWord
          -> Signal' BClkClock Bit
          -> Signal' FFTClock Bool
          -> Signal' FFTClock Bool
          -> (Signal' FFTClock FWord, Signal' FFTClock Bit)
fir17sync lAdcData pulseAdc48KHz enable highorlow = (mux enable filtered lAdcDataS
                                                    ,mux enable fPulse pulseAdc48KHzS
                                                    )
  where
    lAdcDataS         = wordSynchronize bclkClock fftClock 0 lAdcData
    pulseAdc48KHzS    = wordSynchronize bclkClock fftClock 0 pulseAdc48KHz
    (filtered,fPulse) = fir17 lAdcDataS pulseAdc48KHzS highorlow



-- Test the filter
-- res = simulate fir17 firdata

-- firdata  = zip3 (concatMap datavals firdata') enables (repeat True)
-- firdata' = [(-2)^15-1,(2)^15-1,(-2)^15-1,(2)^15-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
-- datavals  = replicate 5
-- enables   = [High,High,Low,Low,Low] ++ enables

