module DE1 where

import CLaSH.Prelude
import CLaSH.Prelude.Explicit

import DE1Types
import I2C
import I2C.Types
import AudioConf
import AudioController
import FFT
import FIR
import Keyboard
import ToneGeneration
import Mixer
import SpectrumDisplay
import Utils

{-# ANN topEntity
  (defTop
    { t_name     = "de1"
    , t_inputs   = ["KEY1"
                   ,"PS2_DAT"
                   ,"PS2_CLK"
                   ,"I2C_SDAT"
                   ,"sramDataIn"
                   ,"AUD_ADCLRCK"
                   ,"AUD_DACLRCK"
                   ,"AUD_ADCDAT"
                   ,"SW0"
                   ,"SW1"]
    , t_outputs  = ["LEDG0"
                   ,"LEDG1"
                   ,"AUD_DACDAT"
                   ,"sclo"
                   ,"I2C_SCLK"
                   ,"sdao"
                   ,"sdaoEn"
                   ,"HEX3"
                   ,"HEX2"
                   ,"HEX1"
                   ,"HEX0"
                   ,"VGA_HS"
                   ,"VGA_VS"
                   ,"VGA_R"
                   ,"VGA_G"
                   ,"VGA_B"
                   ,"sramDataOut"
                   ,"SRAM_ADDR"
                   ,"sramWe_n"
                   ,"SRAM_OE_N"
                   ,"SRAM_UB_N"
                   ,"SRAM_LB_N"
                   ,"SRAM_CE_N"
                   ]
    , t_extraIn  = [ ("CLOCK_50", 1)
                   , ("KEY0", 1)
                   ]
    , t_clocks    = [ ClockSource {c_name  = "fftpll"
                                  ,c_inp   = [("inclk0","CLOCK_50(0)")]
                                  ,c_outp  = [("c0",show systemClock)
                                             ,("c1",show fftClock)
                                             ]
                                  ,c_reset = Just ("areset","NOT KEY0(0)")
                                  ,c_lock  = "locked"
                                  ,c_sync  = True
                                  }
                    ]
    }) #-}
topEntity
  :: (Signal' ('Clk "system" 1000) Bool,
      Signal' KBClock (Bit, Bit),
      Signal' ('Clk "system" 1000) Bit,
      Signal' ('Clk "system" 1000) (BitVector 16),
      Signal' ('Clk "bclk" 3000) (Bit, Bit, Bit),
      Signal' BClkClock Bool,
      Signal' BClkClock Bool)
     -> (Signal' ('Clk "system" 1000) Bool,
         Signal' ('Clk "system" 1000) Bool,
         Signal' ('Clk "bclk" 3000) (BitVector 1),
         Signal I2COut,
         (Signal' ('Clk "system" 1000) (BitVector 7),
          Signal' ('Clk "system" 1000) (BitVector 7),
          Signal' ('Clk "system" 1000) (BitVector 7),
          Signal' ('Clk "system" 1000) (BitVector 7)),
         (Signal' ('Clk "system" 1000) (BitVector 1),
          Signal' ('Clk "system" 1000) (BitVector 1),
          Signal' ('Clk "system" 1000) (BitVector 4),
          Signal' ('Clk "system" 1000) (BitVector 4),
          Signal' ('Clk "system" 1000) (BitVector 4)),
         Signal'
           ('Clk "system" 1000)
           (BitVector 16, BitVector 18, Bit, Bit, Bit, Bit, Bit))
topEntity (rst,kbdata,sdaI,sramIn,audioClks,filterEnable,highOrLow) = (done,fault,dacDat,i2cO,hexdisps,vgaOut,sramOut)
  where
    (dout,hostAck,busy,al,ackOut,i2cO)           = i2c rst (signal True) 99 start stop (signal False) write (signal False) din (bundle (scl,sdaI))
    (hexdisps,key)                               = keyboard kbdata
    sine                                         = toneGeneration pulseDac48KHz key
    (_,sclOen,_,_)                               = unbundle i2cO
    scl                                          = pack <$> sclOen
    (start,stop,write,din,done,fault)            = audioConfig (rst,signal True,hostAck,ackOut,al)
    (pulseAdc48KHz,pulseDac48KHz,adcData,dacDat) = audioCtrl (firDataL,firDataR,done,audioClks)
    (lAdcData,rAdcData)                          = unbundle adcData
    (mixChannelL,mixChannelR)                    = mixer ((lAdcData,unsafeSynchronizer systemClock bclkClock sine)
                                                         ,(rAdcData,unsafeSynchronizer systemClock bclkClock sine))
    (firDataL,pulseAdc48KHzDL)                   = fir17sync mixChannelL pulseAdc48KHz filterEnable highOrLow
    (firDataR,_)                                 = fir17sync mixChannelR pulseAdc48KHz filterEnable highOrLow
    firDataLS                                    = wordSynchronize bclkClock fftClock 0 firDataL
    pulseAdc48KHzDLS                             = wordSynchronize bclkClock fftClock 0 pulseAdc48KHzDL
    fftData                                      = fftfull (bundle (firDataLS,pulseAdc48KHzDLS))
    (sramOut,vgaOut)                             = spectrumDisplay sramIn fftData
