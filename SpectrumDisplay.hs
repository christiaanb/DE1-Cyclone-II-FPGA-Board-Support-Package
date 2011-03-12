{-# LANGUAGE Arrows, RecordWildCards #-}
module SpectrumDisplay (spectrumDisplay) where

import CLasH.HardwareTypes
import DE1Types
import Utils
import SRAMcontroller
import VGAController

data SpecStateMachine = Fill | Purge | Sync
  deriving Eq

data SpectrumS = SpectrumS { pixelOut      :: Vector D8 Bit
                           , pixelAddrR    :: Unsigned D19
                           , pixelAddrW    :: Unsigned D19
                           , pixelWrite    :: Bool
                           , pixelIn       :: Vector D8 Bit
                           , fftNewP       :: Bool
                           , fftCounter    :: Unsigned D12
                           , fftColCnt     :: Unsigned D12
                           , stateM        :: SpecStateMachine
                           , fftDataR      :: Vector D128 (Signed D18)
                           , fftAddrR      :: Vector D128 (Unsigned D7)
                           , dataCntr      :: Unsigned D12
                           , hsyncP        :: Bit
                           , lineCounter   :: Unsigned D12
                           }

type SpectrumI = (Unsigned D12, Unsigned D12, Unsigned D12, Unsigned D12, Bit, Bit, Bit, Unsigned D12, Unsigned D12, Signed D18, Unsigned D7, Bool, Vector D8 Bit)
type SpectrumO = (Vector D4 Bit, Vector D4 Bit, Vector D4 Bit, (Vector D8 Bit, Vector D19 Bit, Bool))

spectrumDisplay = proc (sramIn,fftData) -> do
  rec ((hsync,vsync),inDisplayArea,counterX,counterY) <- vgaController  -< (800,56,120,64,600,37,6,23)
      (fftVal,fftAddr,fftNew)  <- comp synchronize fftDataInit sysclock -< fftData
      (vgaR,vgaG,vgaB,sramCtrl)                       <- spectrumCtrl   -< (800,600,100,127,hsync,vsync,inDisplayArea,counterX,counterY,fftVal,fftAddr,fftNew,sramData)
      (sramData,sramOut)                              <- sramController -< (sramIn,sramCtrl)
  returnA -< (sramOut,(hsync,vsync,vgaR,vgaG,vgaB))

fftDataInit = vcopy (0,0,False)

spectrumCtrl = comp spectrumCtrlT spectrumInit sysclock

spectrumInit :: SpectrumS
spectrumInit = SpectrumS { hsyncP  = High
                         , fftNewP = False
                         , fftCounter = 0
                         , pixelWrite = False
                         , pixelAddrR = 0
                         , pixelAddrW = 0
                         , pixelIn = vcopy Low
                         , pixelOut = vcopy Low
                         , fftColCnt = 0
                         , stateM = Sync
                         , fftDataR = vcopy 0
                         , fftAddrR = vcopy 0
                         , dataCntr = 0
                         , lineCounter = 0
                         }

spectrumCtrlT :: State SpectrumS -> SpectrumI -> (State SpectrumS, SpectrumO)
spectrumCtrlT (State s@(SpectrumS{..})) inp = (State s', outp)
  where
    (hsize,vsize,yoffset,fftSamples,hsync,vsync,inDisplayArea,counterX,counterY,fftData,fftAddr,fftNew,sramData) = inp
    outp = (vgaR,vgaG,vgaB,(pixelOut,pixelAddr,pixelWrite))
    
    -- Update registers
    s' = stateMachine { hsyncP     = hsyncP'
                      , fftNewP    = fftNewP'
                      , fftCounter = fftCounter'
                      , pixelWrite = pixelWrite'
                      , pixelAddrR = pixelAddrR'
                      , pixelIn    = pixelIn'
                      , lineCounter = fftSamples - (shiftR counterY 2)
                      }
    
    -- Detect new hsync
    hsyncP'      = hsync
    hsyncFalling = hsyncP == High && hsync == Low
    
    -- Detect new fft sample
    fftNewP'     = fftNew
    fftNewRising = (not fftNewP) && fftNew
    
    -- Up fft sample counter when new sample arrives until complete fft is received, then reset   
    fftComplete = fftCounter == fftSamples
    
    fftCounter' = if fftNewRising then
        if fftComplete then 0 else fftCounter + 1
      else
        fftCounter
    
    -- FFT Buffer is full if a complete FFT column has been stored
    bufferFull  = dataCntr == fftSamples
    
    -- FFT Buffer is empty when a complete FFT column is purged
    bufferEmpty = dataCntr == 0
        
    stateMachine = case stateM of
      -- Fill state: fill up FFT column
      Fill -> if bufferFull then           -- If buffer if full, go to the Purge state
                s { stateM = Purge }
              else if fftNewRising then -- Otherwise shift in the current data and address values
                s { fftDataR = fftData +>> fftDataR
                  , fftAddrR = fftAddr +>> fftAddrR
                  , dataCntr = dataCntr + 1
                  }
              else
                s
      
      -- Purge state: purge the content of the FFT column
      Purge -> if bufferEmpty then         -- If buffer is full go to Sync state
                s { stateM = Sync
                  -- Go to the next column
                  , fftColCnt = if fftColCnt < (hsize - 1) then fftColCnt + 1 else 0
                  }
              else if hsyncFalling then -- Otherwise write top of the FFT column to the screen
                s { fftDataR   = 0 +>> fftDataR
                  , fftAddrR   = 0 +>> fftAddrR
                  , pixelOut   = encodePixel (vlast fftDataR)
                  , pixelAddrW = ((resizeUnsigned hsize) * (resizeUnsigned (vlast fftAddrR))) + (resizeUnsigned fftColCnt)
                  , dataCntr   = dataCntr - 1
                  }
              else
                s
      
      -- Sync state: wait until current FFT is done before filling up FFT buffer          
      Sync -> if fftComplete then
                s { stateM = Fill }
              else
                s
        
    -- Pixel read address is X + Y counter (+1 delay)
    pixelAddrR' :: Unsigned D19
    pixelAddrR' = (resizeUnsigned counterX) + ((resizeUnsigned lineCounter) * (resizeUnsigned hsize))
    
    -- Only write pixels on horizontal blank period and when we are purging an fft column buffer
    pixelWrite' = (hsyncP == Low) && (stateM == Purge)
    
    -- Determine pixel addr
    pixelAddr = u2bv (if pixelWrite then pixelAddrW else pixelAddrR)
    
    -- Read pixel from SRAM
    pixelIn'                          = sramData                       
    (pixelRed, pixelGreen, pixelBlue) = decodePixel pixelIn
    
    -- spectrum is diplayed over entire horizal span
    showSpectrum = if counterY > 255 && counterY < 512  then High else Low 
    
    vgaR = vmap (hwand (inDisplayArea `hwand` showSpectrum)) (vreverse pixelRed)
    vgaG = vmap (hwand (inDisplayArea `hwand` showSpectrum)) (vreverse pixelGreen)
    vgaB = vmap (hwand (inDisplayArea `hwand` showSpectrum)) (vreverse pixelBlue)
    

encodePixel :: Signed D18 -> Vector D8 Bit
encodePixel val = (u2bv pixelval)
  where
    pixelval | val < 3     && val > (-2)     = 0
             | val < 5     && val > (-5)     = 1 
             | val < 11    && val > (-11)    = 2
             | val < 24    && val > (-24)    = 3
             | val < 51    && val > (-51)    = 4
             | val < 112   && val > (-112)   = 5
             | val < 245   && val > (-245)   = 6
             | val < 537   && val > (-537)   = 7
             | val < 1177  && val > (-1177)  = 8
             | val < 2581  && val > (-2581)  = 9
             | val < 5661  && val > (-5661)  = 10
             | val < 12417 && val > (-12417) = 11
             | otherwise                     = 12

decodePixel :: Vector D8 Bit -> (Vector D4 Bit, Vector D4 Bit, Vector D4 Bit)
decodePixel val = (r,g,b)
  where
    val' = bv2u val
    (b,g,r) = case val' of
      0  -> (u2bv 0 , u2bv 0 , u2bv 0 )
      1  -> (u2bv 0 , u2bv 0 , u2bv 1 )
      2  -> (u2bv 0 , u2bv 0 , u2bv 3 )
      3  -> (u2bv 0 , u2bv 0 , u2bv 7 )
      4  -> (u2bv 0 , u2bv 0 , u2bv 15)
      5  -> (u2bv 0 , u2bv 1 , u2bv 15)
      6  -> (u2bv 0 , u2bv 3 , u2bv 15)
      7  -> (u2bv 0 , u2bv 7 , u2bv 15)
      8  -> (u2bv 0 , u2bv 15, u2bv 15)
      9  -> (u2bv 1 , u2bv 15, u2bv 15)
      10 -> (u2bv 3 , u2bv 15, u2bv 15)
      11 -> (u2bv 7 , u2bv 15, u2bv 15)
      _  -> (u2bv 15, u2bv 15, u2bv 15)
  