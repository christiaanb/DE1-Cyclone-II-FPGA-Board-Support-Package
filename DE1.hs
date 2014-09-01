-- {-# LANGUAGE Arrows #-}
module DE1 where

-- import CLasH.HardwareTypes
-- import DE1Types
-- import I2CController
-- import AudioConf
-- import Utils
-- import AudioController
-- import ToneGeneration
-- import Keyboard
-- import SpectrumDisplay
-- import FFT
-- import FIR
-- import Mixer

import CLaSH.Prelude
import CLaSH.Prelude.Explicit

import DE1Types
import I2CController
import AudioConf
import AudioController
import Keyboard
import Utils

topEntity
  :: (CSignal ('Clk "system" 1000) Bool,
      CSignal KBClock (Bit, Bit),
      CSignal ('Clk "system" 1000) (BitVector 1),
      CSignal ('Clk "bclk" 3000) (Bit, Bit, Bit))
     -> (CSignal ('Clk "system" 1000) Bool,
         CSignal ('Clk "system" 1000) Bool,
         CSignal ('Clk "bclk" 3000) (BitVector 1),
         CSignal ('Clk "system" 1000) (Bit, Bool, Bit, Bool),
         (CSignal ('Clk "system" 1000) (BitVector 7),
          CSignal ('Clk "system" 1000) (BitVector 7),
          CSignal ('Clk "system" 1000) (BitVector 7),
          CSignal ('Clk "system" 1000) (BitVector 7)))
topEntity (rst,kbdata,sdaI,audioClks) = (done,fault,dacDat,i2cO,hexdisps)
  where
    (dout,hostAck,busy,al,ackOut,i2cO)           = i2cController (rst,signal True,99,start,stop,signal False,write,signal low,din,sUnwrap (scl,sdaI))
    (_,sclOen,_,_)                               = sWrap i2cO
    scl                                          = enable <$> sclOen
    (start,stop,write,din,done,fault)            = audioConfig (rst,not <$> rst,hostAck,ackOut,al)
    (pulseAdc48KHz,pulseDac48KHz,adcData,dacDat) = audioCtrl (lAdcData,rAdcData,done,audioClks)
    (lAdcData,rAdcData)                          = sWrap (wordSynchronize bclkClock systemClock (0,0) adcData)
    (hexdisps,key)                               = keyboard kbdata


-- {-# ANN de1 TopEntity #-}
-- de1 = proc (rst,kbdata,sdaI,sramIn,audioClks,firEnable,highOrLow) -> do
--   rec (dout,hostAck,busy,al,ackOut,i2cO@(sclO,sclOen,sdaO,sdaOen)) <- i2cController   -< (rst,True,99,start,stop,False,write,Low,din,(scl,sdaI))
--       (hexdisps,key)                                               <- keyboard        -< kbdata
--       sine                                                         <- toneGeneration  -< (pulseDac48KHz,key)
--       scl                                                          <- (arr enable)    -< sclOen
--       (start,stop,write,din,done,fault)                            <- audioConfig     -< (rst,not rst,hostAck,ackOut,al)
--       (pulseAdc48KHz,pulseDac48KHz,(lAdcData,rAdcData),dacDat)     <- audioCtrl       -< (firDataL,firDataR,done,audioClks)
--       (mixChannelL,mixChannelR)                                    <- mixer           -< ((lAdcData,sine), (rAdcData,sine))
--       (firDataL,pulseAdc48KHzDL)                                   <- fir17sync       -< (mixChannelL,pulseAdc48KHz,firEnable,highOrLow)
--       (firDataR,pulseAdc48KHzDR)                                   <- fir17sync       -< (mixChannelR,pulseAdc48KHz,firEnable,highOrLow)
--       -- fftData                                                      <- fftGen          -< (firDataL, firDataR, pulseAdc48KHz, 127)
--       fftData                                                      <- fftsync         -< (firDataL,pulseAdc48KHzDL)
--       (sramOut,vgaOut)                                             <- spectrumDisplay -< (sramIn,fftData)
--   returnA -< (done,fault,dacDat,i2cO,hexdisps,vgaOut,sramOut)
