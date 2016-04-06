{-# LANGUAGE RecordWildCards #-}

module AudioController
  (audioCtrl)
where

import CLaSH.Prelude
import CLaSH.Prelude.Explicit
import DE1Types
import Utils

data AudioCtrlS = ACS { lDacCount     :: Index 26
                      , rDacCount     :: Index 26
                      , lAdcCount     :: Index 26
                      , rAdcCount     :: Index 26
                      , pulseAdc48KHz :: Bit
                      , pulseDac48KHz :: Bit
                      , dacData       :: Vec 48 Bit
                      , lAdcData      :: Vec 24 Bit
                      , rAdcData      :: Vec 24 Bit
                      , lAdcDataS     :: BitVector 18
                      , rAdcDataS     :: BitVector 18
                      }

type AudioCtrlI = ((Signed 18, Signed 18, Bool), (Bit,Bit,Bit))
type AudioCtrlO = (Bit,Bit,(Signed 18, Signed 18), Bit)

audioCtrl (ldata,rdata,wEn,clks) = outp
  where
    wEnS     = wordSynchronize systemClock bclkClock False wEn
    outp     = mealyB' bclkClock audioCtrlT audioCtrlInit (bundle' bclkClock (ldata,rdata,wEnS),clks)

audioCtrlInit :: AudioCtrlS
audioCtrlInit = ACS 0 0 0 0 low low (repeat low) (repeat low) (repeat low) 0 0

audioCtrlT :: AudioCtrlS -> AudioCtrlI -> (AudioCtrlS, AudioCtrlO)
audioCtrlT s@(ACS{..}) inp = (dacSm, outp)
  where
    ((ldata,rdata,wEn),(adcLRclk,dacLRclk,adcDat)) = inp
    outp = (pulseAdc48KHz,pulseDac48KHz,(unpack lAdcDataS, unpack rAdcDataS),head dacData)

    sd = s { pulseAdc48KHz = low, pulseDac48KHz = low }

    ldata' :: Vec 24 Bit
    ldata' = unpack (pack ldata ++# (0 :: BitVector 6))

    rdata' :: Vec 24 Bit
    rdata' = unpack (pack rdata ++# (0 :: BitVector 6))

    adcSm = if adcLRclk == low then
        let sadcl = sd { lAdcCount = lAdcCount + 1, rAdcCount = 0}
        in
          case lAdcCount of
            0         -> sadcl { pulseAdc48KHz = high, lAdcDataS = pack (take d18 lAdcData), rAdcDataS = pack (take d18 rAdcData)}
            25        -> sd
            otherwise -> sadcl { lAdcData = lAdcData <<+ adcDat }
      else -- adcLRclk == high
        let sadcr = s { rAdcCount = rAdcCount + 1, lAdcCount = 0 }
        in
          case rAdcCount of
            0         -> sadcr
            25        -> sd
            otherwise -> sadcr { rAdcData = rAdcData <<+ adcDat }

    dacSm = if dacLRclk == low then
        let sdacl = adcSm { lDacCount = lDacCount + 1, rDacCount = 0 }
        in
          case lDacCount of
            0         -> sdacl { dacData = ldata' ++ rdata', pulseDac48KHz = high }
            25        -> adcSm
            otherwise -> sdacl { dacData = dacData <<+ low }
      else -- dacLRclk == high
        let sdacr = adcSm { rDacCount = rDacCount + 1, lDacCount = 0 }
        in
          case rDacCount of
            0         -> sdacr
            25        -> adcSm
            otherwise -> sdacr { dacData = dacData <<+ low }
