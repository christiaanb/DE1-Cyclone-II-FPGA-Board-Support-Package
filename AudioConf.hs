{-# LANGUAGE Arrows, RecordWildCards, TemplateHaskell #-}
module AudioConf ( 
    audioConfig 
  ) where

import CLasH.HardwareTypes
import DE1Types
import Utils
import Debug.Trace

data AudioStateMachine = AUDIOCONFena  | 
                         AUDIOCONFaddr | AUDIOCONFaddrAck |
                         AUDIOCONFreg  | AUDIOCONFregAck  | 
                         AUDIOCONFdata | AUDIOCONFdataAck | 
                         AUDIOCONFstop

data AudioConfS = AudioConfS { audioConfStateM :: AudioStateMachine
                             , start           :: Bool
                             , stop            :: Bool
                             , write           :: Bool
                             , din             :: Vector D8 Bit
                             , lutIndex        :: Index D12
                             , fault           :: Bool
                             }

type AudioConfI = (Bool,Bool,Bool,Bit,Bool)
type AudioConfO = (Bool,Bool,Bool,Vector D8 Bit,Bool,Bool)

audioConfig = proc inp -> do
  outp <- (comp audioConfigT audioConfInit sysclock) -< inp
  returnA -< outp

audioConfInit :: AudioConfS
audioConfInit = AudioConfS { audioConfStateM = AUDIOCONFena
                           , start           = False
                           , stop            = False
                           , write           = False
                           , din             = vcopy Low
                           , lutIndex        = 0
                           , fault           = False
                           }

audioConfigT :: State AudioConfS -> AudioConfI -> (State AudioConfS, AudioConfO)
audioConfigT (State s@(AudioConfS{..})) inp = (State s', outp)
  where
    (rst,ena,cmdAck,rxAck,al) = inp
    outp = (start,stop,write,din,done,fault)
    
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
          AUDIOCONFaddr -> s {audioConfStateM = AUDIOCONFaddrAck, start = True, write = True, din = u2bv i2cSlvAddr}
          
          -- Wait until I2C controller is done; if so: clear start and write bits
          AUDIOCONFaddrAck -> if success then
              s {audioConfStateM = AUDIOCONFreg, start = False, write = False}
            else
              s
              
          -- Read acknowledge, should be low; if so: write register address, set write bit, and deassert fault
          AUDIOCONFreg -> if (rxAck == Low) then -- trace "Add Acknowledged" $
              s {audioConfStateM = AUDIOCONFregAck, write = True, din = u2bv (fst lutData), fault = False}
            else
              -- Otherwise a fault occured: start over, and assert fault bit
              s {audioConfStateM = AUDIOCONFena, fault = True}
              
          
          -- Wait until I2C controller is done; if so: clear write bit
          AUDIOCONFregAck -> if success then
              s {audioConfStateM = AUDIOCONFdata, write = False}
            else
              s
              
          -- Read acknowledge, should be low; if so: write data to register, set write and stop bit, and deassert fault
          AUDIOCONFdata -> if (rxAck == Low) then -- trace "Reg Acknowledged" $
              s {audioConfStateM = AUDIOCONFdataAck, write = True, stop = True, din = snd lutData, fault = False}
            else
              -- Otherwise a fault occured: start over, and assert fault bit
              s {audioConfStateM = AUDIOCONFena, fault = True}
              
          -- Wait until I2C controller is done; if so: clear stop and write bit
          AUDIOCONFdataAck -> if success then
              s {audioConfStateM = AUDIOCONFstop, stop = False, write = False}
            else
              s
              
          -- Read acknowledge, shoulf be low; if so: go to next cycle by updating lutIndex, and deassert fault
          AUDIOCONFstop -> if (rxAck == Low) then -- trace "Val Acknowledged" $
              s {audioConfStateM = AUDIOCONFena, lutIndex = lutIndex + 1, fault = False}
            else
              -- Otherwise a fault occured: start over, and assert fault
              s {audioConfStateM = AUDIOCONFena, fault = True}
    
    lutData
      -- R15 Reset Device:
      | lutIndex == 0  = (0x1E,vcopy Low) -- 8:0 RESET = 00000000 => Resets device
      
      -- R0 Left line In:
      -- 8 Sim Load RIN = 0 => Disable simultaneous Load
      | lutIndex == 1  = (0x00,$(vTH [ Low                      -- 7   LIN Mute ADC = Disable Mute
                                     , Low,Low                  -- 6:5 N/A
                                     , High,High,High,High,High -- 4:0 LINVOL       = +12dB
                                     ]))
                                     
      -- R1 Right line In:
      -- 8 Sim Load LIN = 0 => Disable simultaneous Load
      | lutIndex == 2  = (0x02,$(vTH [ Low                      -- 7   RIN Mute ADC = Disable Mute
                                     , Low,Low                  -- 6:5 N/A
                                     , High,High,High,High,High -- 4:0 RINVOL       = +12dB
                                     ]))
                                     
      -- R2 Left Headphone Out:
      -- 8   Sim Load RHD = 0 => Disable simultaneous Load
      | lutIndex == 3  = (0x04,$(vTH [ High                              -- 7   Zero Cross Detec = Enable
                                     , High,High,High,High,Low,Low,High  -- 6:0 LHPVOL           = +0dB
                                     ]))
                                     
      -- R3 Right Headphone Out:
      -- 8   Sim Load LHD = 0 => Disable simultaneous Load
      | lutIndex == 4  = (0x06,$(vTH [ High                              -- 7   Zero Cross Detec = Enable
                                     , High,High,High,High,Low,Low,High  -- 6:0 RHPVOL           = +0dB
                                     ]))
                                     
      -- R4 Analogue Audio Path Control:      
      | lutIndex == 5  = (0x08,$(vTH [ Low,Low -- 7:6 SIDEATT[1:0] = -6dB
                                     , Low     -- 5   SIDETONE     = Disable Side Tone
                                     , High    -- 4   DACSEL       = Select DAC
                                     , Low     -- 3   BYPASS       = Disable Bypass
                                     , Low     -- 2   INSEL        = Line Input Select to ADC
                                     , High    -- 1   MUTEMIC      = Mute Mic Input to ADC
                                     , Low     -- 0   MICBOOST     = Disable Microphone Input LevelBoost
                                     ]))
                                     
      -- R5 Digital Audio Path Control:
      | lutIndex == 6  = (0x0A,$(vTH [ Low,Low,Low -- 7:5 N/A
                                     , Low         -- 4   HPOR       = Clear DC offset when High Pass Filter Disabled
                                     , Low         -- 3   DACMU      = Disable DAC Soft Mute
                                     , High,High   -- 2:1 DEEMP[1:0] = De-emphasis Control: 48kHz
                                     , Low         -- 0   ADCHPD     = Enable ADC High Pass Filter
                                     ]))
                                     
      -- R6 Power Down Control:
      | lutIndex == 7  = (0x0C,$(vTH [ Low  -- 7 POWEROFF = Disable POWEROFF mode
                                     , Low  -- 6 CLKOUTPD = Disable CLKOUT power down
                                     , Low  -- 5 OSCPD    = Disable Occilator power down
                                     , Low  -- 4 OUTPD    = Disable outputs power down
                                     , Low  -- 3 DACPD    = Disable DAC power down
                                     , Low  -- 2 ADCPD    = Disable ADC power down
                                     , Low  -- 1 MICPD    = Disable Mic Input on Bias powerdown
                                     , Low  -- 0 LINEINPD = Disable line input power down
                                     ]))
                                     
      -- R7 Digital Audio Interface format:
      | lutIndex == 8  = (0x0E,$(vTH [ Low       -- 7   BCLKINV     = Don't invert BCLK
                                     , High      -- 6   MS          = Master Mode
                                     , Low       -- 5   LRSWAP      = Right Channel DAC Data Right
                                     , Low       -- 4   LRP         = I2S mode: Right Channel DAC data when DACLRC high
                                     , High,Low  -- 3:2 IWL[1:0]    = Input Audio Data Bit Length: 24 Bits
                                     , High,Low  -- 1:0 FORMAT[1:0] = Audio Data Format: I2S Format, MSB-First left-1 justified
                                     ]))
                                     
      -- R8 Sampling Control:
      | lutIndex == 9  = (0x10,$(vTH [ Low             -- 7   CLKODIV2   = CLOCKOUT is Core Clock
                                     , Low             -- 6   CLKIDIV2   = Core clock is MCLK
                                     , Low,Low,Low,Low -- 5:2 SR[3:0]    = Sample rate control => ADC: 48kHz; DAC: 48kHz; Digital Filter Type: 0
                                     , Low             -- 1   BOSR       = Base Over-Sampling Rate => USB mode: 250fs
                                     , High            -- 0   USB/Normal = Mode Select: Normal mode (250/272fs)
                                     ]))
                                     
      -- R9 Active Control
      | lutIndex == 10 = (0x12,$(vTH [ Low,Low,Low,Low,Low,Low,Low -- 7:1 N/A
                                     , High                        -- 0   ACTIVE => Activate Inferace
                                     ]))

      -- ERROR STATE => RESET DEVICE!
      -- R15 Reset Device:
      | otherwise      = (0x1E,vcopy Low) -- 8:0 RESET = 00000000 => Resets device
