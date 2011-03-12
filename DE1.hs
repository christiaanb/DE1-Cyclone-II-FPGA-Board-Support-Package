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

{-# ANN de1 TopEntity #-}
de1 = proc (rst,kbdata,sdaI,sramIn,audioClks) -> do
  rec (dout,hostAck,busy,al,ackOut,i2cO@(sclO,sclOen,sdaO,sdaOen)) <- i2cController   -< (rst,True,99,start,stop,False,write,Low,din,(scl,sdaI))
      scl                                                          <- (arr enable)    -< sclOen
      (start,stop,write,din,done,fault)                            <- audioConfig     -< (rst,not rst,hostAck,ackOut,al)
      (pulseAdc48KHz,pulseDac48KHz,(lAdcData,rAdcData),dacDat)     <- audioCtrl       -< (sine,sine,done,audioClks)
      sine                                                         <- toneGeneration  -< (pulseDac48KHz,key)
      (hexdisps,key)                                               <- keyboard        -< kbdata
      -- fftData                                                      <- fftGen          -< (lAdcData, rAdcData, pulseAdc48KHz, 127)
      fftData                                                      <- fftsync         -< (lAdcData, pulseAdc48KHz)
      (sramOut,vgaOut)                                             <- spectrumDisplay -< (sramIn,fftData)
  returnA -< (done,fault,dacDat,i2cO,hexdisps,vgaOut,sramOut)

enable a = if a then High else Low
