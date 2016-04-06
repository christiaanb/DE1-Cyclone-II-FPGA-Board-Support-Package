module FIR where

import CLaSH.Prelude
import CLaSH.Prelude.Explicit
import DE1Types
import Utils

import qualified Data.List as L

fpmult :: FWord -> FWord -> FWord
fpmult i h = resize m'
  where m  = i `times` h :: DWord
        m' = shiftR m 17


-- cutoff freq = 938 Hz
lowpass :: Vec 17 FWord
lowpass = $(v [ 1046, 1565, 3024, 5274, 8007, 10808, 13227, 14863, 15442, 14863, 13227, 10808, 8007,  5274,  3024,  1565,  (1046 :: Signed 18) ] )

highpass :: Vec 17 FWord
highpass = $(v [ 130025, -1565, -3024, -5274, -8007, -10808, -13227, -14863, -15442, -14863, -13227, -10808, -8007,  -5274,  -3024,  -1565,  (-1046 :: Signed 18) ] )


fir17T :: (Vec 17 FWord) -> (FWord, Bool, Vec 17 FWord) -> ((Vec 17 FWord), FWord)
fir17T (us) (x,enable,hs) = (usn, y)
  where
    ms   = map (fpmult x) hs
    usn' = zipWith boundedPlus (us <<+ 0) ms
    usn  = if enable then usn' else us
    y    = head us

-- input range is 2^17-1 .. -2^17
fir17filter = mealyB' bclkClock fir17T (repeat 0)

fir17 :: Signal' BClkClock FWord -> Signal' BClkClock Bit
      -> Signal' BClkClock Bool -> (Signal' BClkClock FWord, Signal' BClkClock Bit)
fir17 audioData syncPulse highorlow = (filteredData,syncPulseD)
  where
    firEnable    = unpack <$> syncPulse
    filteredData = fir17filter (audioData, firEnable, mux highorlow (pure highpass) (pure lowpass))
    syncPulseD   = delayN bclkClock (singleton 0) syncPulse

fir17sync :: Signal' BClkClock FWord
          -> Signal' BClkClock Bit
          -> Signal' BClkClock Bool
          -> Signal' BClkClock Bool
          -> (Signal' BClkClock FWord, Signal' BClkClock Bit)
fir17sync lAdcData pulseAdc48KHz enable highorlow = (mux enable filtered lAdcData
                                                    ,mux enable fPulse   pulseAdc48KHz
                                                    )
  where
    (filtered,fPulse) = fir17 lAdcData pulseAdc48KHz highorlow
