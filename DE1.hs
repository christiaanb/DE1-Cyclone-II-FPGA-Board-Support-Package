{-# LANGUAGE Arrows #-}
module DE1 where

import CLasH.HardwareTypes
import DE1Types
import I2CController
import AudioConf
import Utils
import AudioController
import ToneGeneration
import Keyboard
import SpectrumDisplay
-- import FFTGen
import FFT
import FIR
import Mixer

{-# ANN de1 TopEntity #-}
de1 = proc (rst,kbdata,sdaI,sramIn,audioClks,firEnable,highOrLow) -> do
  rec (dout,hostAck,busy,al,ackOut,i2cO@(sclO,sclOen,sdaO,sdaOen)) <- i2cController   -< (rst,True,99,start,stop,False,write,Low,din,(scl,sdaI))
      (hexdisps,key)                                               <- keyboard        -< kbdata
      sine                                                         <- toneGeneration  -< (pulseDac48KHz,key)
      scl                                                          <- (arr enable)    -< sclOen
      (start,stop,write,din,done,fault)                            <- audioConfig     -< (rst,not rst,hostAck,ackOut,al)
      (pulseAdc48KHz,pulseDac48KHz,(lAdcData,rAdcData),dacDat)     <- audioCtrl       -< (firDataL,firDataR,done,audioClks)
      (mixChannelL,mixChannelR)                                    <- mixer           -< ((lAdcData,sine), (rAdcData,sine))
      (firDataL,pulseAdc48KHzDL)                                   <- fir17sync       -< (mixChannelL,pulseAdc48KHz,firEnable,highOrLow)
      (firDataR,pulseAdc48KHzDR)                                   <- fir17sync       -< (mixChannelR,pulseAdc48KHz,firEnable,highOrLow)
      -- fftData                                                      <- fftGen          -< (firDataL, firDataR, pulseAdc48KHz, 127)
      fftData                                                      <- fftsync         -< (firDataL,pulseAdc48KHzDL)
      (sramOut,vgaOut)                                             <- spectrumDisplay -< (sramIn,fftData)
  returnA -< (done,fault,dacDat,i2cO,hexdisps,vgaOut,sramOut)

enable a = if a then High else Low
