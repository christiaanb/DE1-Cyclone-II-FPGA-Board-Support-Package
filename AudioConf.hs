{-# LANGUAGE RecordWildCards #-}
module AudioConf (
    audioConfig
  ) where

import CLaSH.Prelude
import DE1Types
import Utils

data AudioStateMachine = AUDIOCONFena  |
                         AUDIOCONFaddr | AUDIOCONFaddrAck |
                         AUDIOCONFreg  | AUDIOCONFregAck  |
                         AUDIOCONFdata | AUDIOCONFdataAck |
                         AUDIOCONFstop

data AudioConfS = AudioConfS { audioConfStateM :: AudioStateMachine
                             , start           :: Bool
                             , stop            :: Bool
                             , write           :: Bool
                             , din             :: BitVector 8
                             , lutIndex        :: Index 12
                             , fault           :: Bool
                             }

type AudioConfI = (Bool,Bool,Bool,Bit,Bool)
type AudioConfO = (Bool,Bool,Bool,BitVector 8,Bool,Bool)

audioConfig = audioConfigT <^> audioConfInit

audioConfInit :: AudioConfS
audioConfInit = AudioConfS { audioConfStateM = AUDIOCONFena
                           , start           = False
                           , stop            = False
                           , write           = False
                           , din             = 0 --repeat low
                           , lutIndex        = 0
                           , fault           = False
                           }

audioConfigT :: AudioConfS -> AudioConfI -> (AudioConfS, AudioConfO)
audioConfigT s@(AudioConfS{..}) inp = (s', outp)
  where
    (rst,ena,cmdAck,rxAck,al) = inp
    outp = (start,stop,write,din,done,fault)

    i2cSlvAddr :: BitVector 8
    i2cSlvAddr = 0x34

    success = cmdAck && (not al)

    done = lutIndex == 11

    s' = if rst then
        audioConfInit
      else
        case audioConfStateM of
          -- Is the Configuration device enabled, and is it not done, then continue
          AUDIOCONFena  -> if ena && (not done) then
              s {audioConfStateM = AUDIOCONFaddr}
            else
              s

          -- Generate Start command & Write I2C slave Address
          AUDIOCONFaddr -> s {audioConfStateM = AUDIOCONFaddrAck, start = True, write = True, din = i2cSlvAddr}

          -- Wait until I2C controller is done; if so: clear start and write bits
          AUDIOCONFaddrAck -> if success then
              s {audioConfStateM = AUDIOCONFreg, start = False, write = False}
            else
              s

          -- Read acknowledge, should be low; if so: write register address, set write bit, and deassert fault
          AUDIOCONFreg -> if (rxAck == low) then -- trace "Add Acknowledged" $
              s {audioConfStateM = AUDIOCONFregAck, write = True, din = pack (fst lutData), fault = False}
            else
              -- Otherwise a fault occured: start over, and assert fault bit
              s {audioConfStateM = AUDIOCONFena, fault = True}


          -- Wait until I2C controller is done; if so: clear write bit
          AUDIOCONFregAck -> if success then
              s {audioConfStateM = AUDIOCONFdata, write = False}
            else
              s

          -- Read acknowledge, should be low; if so: write data to register, set write and stop bit, and deassert fault
          AUDIOCONFdata -> if (rxAck == low) then -- trace "Reg Acknowledged" $
              s {audioConfStateM = AUDIOCONFdataAck, write = True, stop = True, din = pack (snd lutData), fault = False}
            else
              -- Otherwise a fault occured: start over, and assert fault bit
              s {audioConfStateM = AUDIOCONFena, fault = True}

          -- Wait until I2C controller is done; if so: clear stop and write bit
          AUDIOCONFdataAck -> if success then
              s {audioConfStateM = AUDIOCONFstop, stop = False, write = False}
            else
              s

          -- Read acknowledge, shoulf be low; if so: go to next cycle by updating lutIndex, and deassert fault
          AUDIOCONFstop -> if (rxAck == low) then -- trace "Val Acknowledged" $
              s {audioConfStateM = AUDIOCONFena, lutIndex = lutIndex + 1, fault = False}
            else
              -- Otherwise a fault occured: start over, and assert fault
              s {audioConfStateM = AUDIOCONFena, fault = True}

    lutData :: (Unsigned 8,Vec 8 Bit)
    lutData
      -- R15 Reset Device:
      | lutIndex == 0  = (0x1E,repeat low) -- 8:0 RESET = 00000000 => Resets device

      -- R0 Left line In:
      -- 8 Sim Load RIN = 0 => Disable simultaneous Load
      | lutIndex == 1  = (0x00,$(v [ low                      -- 7   LIN Mute ADC = Disable Mute
                                   , low,low                  -- 6:5 N/A
                                   , high,high,high,high,high -- 4:0 LINVOL       = +12dB
                                   ]))

      -- R1 Right line In:
      -- 8 Sim Load LIN = 0 => Disable simultaneous Load
      | lutIndex == 2  = (0x02,$(v [ low                      -- 7   RIN Mute ADC = Disable Mute
                                   , low,low                  -- 6:5 N/A
                                   , high,high,high,high,high -- 4:0 RINVOL       = +12dB
                                   ]))

      -- R2 Left Headphone Out:
      -- 8   Sim Load RHD = 0 => Disable simultaneous Load
      | lutIndex == 3  = (0x04,$(v [ high                              -- 7   Zero Cross Detec = Enable
                                   , high,high,high,high,low,low,high  -- 6:0 LHPVOL           = +0dB
                                   ]))

      -- R3 Right Headphone Out:
      -- 8   Sim Load LHD = 0 => Disable simultaneous Load
      | lutIndex == 4  = (0x06,$(v [ high                              -- 7   Zero Cross Detec = Enable
                                   , high,high,high,high,low,low,high  -- 6:0 RHPVOL           = +0dB
                                   ]))

      -- R4 Analogue Audio Path Control:
      | lutIndex == 5  = (0x08,$(v [ low,low -- 7:6 SIDEATT[1:0] = -6dB
                                   , low     -- 5   SIDETONE     = Disable Side Tone
                                   , high    -- 4   DACSEL       = Select DAC
                                   , low     -- 3   BYPASS       = Disable Bypass
                                   , low     -- 2   INSEL        = Line Input Select to ADC
                                   , high    -- 1   MUTEMIC      = Mute Mic Input to ADC
                                   , low     -- 0   MICBOOST     = Disable Microphone Input LevelBoost
                                   ]))

      -- R5 Digital Audio Path Control:
      | lutIndex == 6  = (0x0A,$(v[ low,low,low -- 7:5 N/A
                                  , low         -- 4   HPOR       = Clear DC offset when High Pass Filter Disabled
                                  , low         -- 3   DACMU      = Disable DAC Soft Mute
                                  , high,high   -- 2:1 DEEMP[1:0] = De-emphasis Control: 48kHz
                                  , low         -- 0   ADCHPD     = Enable ADC High Pass Filter
                                  ]))

      -- R6 Power Down Control:
      | lutIndex == 7  = (0x0C,$(v [ low  -- 7 POWEROFF = Disable POWEROFF mode
                                   , low  -- 6 CLKOUTPD = Disable CLKOUT power down
                                   , low  -- 5 OSCPD    = Disable Occilator power down
                                   , low  -- 4 OUTPD    = Disable outputs power down
                                   , low  -- 3 DACPD    = Disable DAC power down
                                   , low  -- 2 ADCPD    = Disable ADC power down
                                   , low  -- 1 MICPD    = Disable Mic Input on Bias powerdown
                                   , low  -- 0 LINEINPD = Disable line input power down
                                   ]))

      -- R7 Digital Audio Interface format:
      | lutIndex == 8  = (0x0E,$(v [ low       -- 7   BCLKINV     = Don't invert BCLK
                                   , high      -- 6   MS          = Master Mode
                                   , low       -- 5   LRSWAP      = Right Channel DAC Data Right
                                   , low       -- 4   LRP         = I2S mode: Right Channel DAC data when DACLRC high
                                   , high,low  -- 3:2 IWL[1:0]    = Input Audio Data Bit Length: 24 Bits
                                   , high,low  -- 1:0 FORMAT[1:0] = Audio Data Format: I2S Format, MSB-First left-1 justified
                                   ]))

      -- R8 Sampling Control:
      | lutIndex == 9  = (0x10,$(v [ low             -- 7   CLKODIV2   = CLOCKOUT is Core Clock
                                   , low             -- 6   CLKIDIV2   = Core clock is MCLK
                                   , low,low,low,low -- 5:2 SR[3:0]    = Sample rate control => ADC: 48kHz; DAC: 48kHz; Digital Filter Type: 0
                                   , low             -- 1   BOSR       = Base Over-Sampling Rate => USB mode: 250fs
                                   , high            -- 0   USB/Normal = Mode Select: Normal mode (250/272fs)
                                   ]))

      -- R9 Active Control
      | lutIndex == 10 = (0x12,$(v [ low,low,low,low,low,low,low -- 7:1 N/A
                                   , high                        -- 0   ACTIVE => Activate Inferace
                                   ]))

      -- ERROR STATE => RESET DEVICE!
      -- R15 Reset Device:
      | otherwise      = (0x1E,repeat low) -- 8:0 RESET = 00000000 => Resets device
