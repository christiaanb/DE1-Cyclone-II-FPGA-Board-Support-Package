{-# LANGUAGE Arrows, RecordWildCards #-}
module FFTGen (fftGen) where

import CLasH.HardwareTypes
import DE1Types
import Utils

type FFTGenI = ((Signed D18, Signed D18, Bit), Unsigned D7)
type FFTGenO = (Signed D18, Unsigned D7, Bool)

data FFTGenS = FFTGenS { pulseAdc48KHzP :: Bit
                       , fftCntr        :: Unsigned D7
                       , fftData        :: Signed D18
                       , flipBit        :: Bool
                       }

fftGen = proc (lAdcData,rAdcData,pulseAdc48KHz,fftSize) -> do
  adcData <- comp synchronize adcDataInit fftClock -< (lAdcData,rAdcData,pulseAdc48KHz)
  outp    <- comp fftGenT fftGenInit fftClock      -< (adcData,fftSize)
  returnA -< outp

adcDataInit = vcopy (0,0,Low)

fftGenInit = FFTGenS Low 0 0 False

fftGenT :: State FFTGenS -> FFTGenI -> (State FFTGenS, FFTGenO)
fftGenT (State s@(FFTGenS{..})) inp = (State s', outp)
  where
    ((lAdcData,rAdcData,pulseAdc48KHz),fftSize) = inp
    outp = (fftData, fftCntr, pulseAdc48KHzRising)
    
    -- update registers
    s' = FFTGenS { pulseAdc48KHzP      = pulseAdc48KHzP'
                 , fftCntr             = fftCntr'
                 , fftData             = fftData'
                 , flipBit             = flipBit'
                 }
    
    -- Detect new pulse
    pulseAdc48KHzP'     = pulseAdc48KHz
    pulseAdc48KHzRising = (pulseAdc48KHzP == Low) && (pulseAdc48KHz == High)
    
    fftFull = fftCntr == fftSize
    
    fftCntr' = if pulseAdc48KHzRising then
        if fftFull then 0 else fftCntr + 1
      else
        fftCntr
    
    flipBit' = if pulseAdc48KHzRising then not flipBit else flipBit
    
    fftData' = if pulseAdc48KHzRising then 
        lAdcData
      else 
        fftData
    