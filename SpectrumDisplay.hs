{-# LANGUAGE Arrows, RecordWildCards #-}
module SpectrumDisplay (spectrumDisplay) where

import CLaSH.Prelude
import CLaSH.Prelude.Explicit
import DE1Types
import Utils
import SRAMcontroller
import VGAController

data SpecStateMachine = Fill | Purge | Sync
  deriving Eq

data SpectrumS = SpectrumS { pixelOut      :: BitVector 8
                           , pixelAddrR    :: Unsigned 19
                           , pixelAddrW    :: Unsigned 19
                           , pixelWrite    :: Bool
                           , pixelIn       :: BitVector 8
                           , fftNewP       :: Bool
                           , fftCounter    :: Unsigned 12
                           , fftColCnt     :: Unsigned 12
                           , stateM        :: SpecStateMachine
                           , fftDataR      :: Vec 128 (Signed 18)
                           , fftAddrR      :: Vec 128 (Unsigned 7)
                           , dataCntr      :: Unsigned 12
                           , hsyncP        :: Bit
                           , lineCounter   :: Unsigned 12
                           }

type SpectrumI = (Unsigned 12, Unsigned 12, Unsigned 12, Unsigned 12, Bit, Bit
                 ,Bit, Unsigned 12, Unsigned 12, Signed 18, Unsigned 7, Bool
                 ,BitVector 8)
type SpectrumO = (BitVector 4, BitVector 4, BitVector 4
                 ,(BitVector 8, BitVector 19, Bool))

spectrumDisplay sramIn fftData = (sramOut,(hsync,vsync,vgaR,vgaG,vgaB))
  where
    (syncs,inDisplayArea,counterX,counterY) = vgaController (800,56,120,64,600,37,6,23)
    (hsync,vsync) = unbundle syncs
    (fftVal,fftAddr,fftNew) = unbundle $ wordSynchronize fftClock systemClock (0,0,False) fftData
    (vgaR,vgaG,vgaB,sramCtrl) = spectrumCtrl (800,600,100,127,hsync,vsync,inDisplayArea,counterX,counterY,fftVal,fftAddr,fftNew,sramData)
    (sramData,sramOut) = unbundle $ sramController (bundle (sramIn,sramCtrl))

fftDataInit = (0,0,False)

spectrumCtrl = mealyB spectrumCtrlT spectrumInit

spectrumInit :: SpectrumS
spectrumInit = SpectrumS { hsyncP  = 0b1
                         , fftNewP = False
                         , fftCounter = 0
                         , pixelWrite = False
                         , pixelAddrR = 0
                         , pixelAddrW = 0
                         , pixelIn = 0
                         , pixelOut = 0
                         , fftColCnt = 0
                         , stateM = Sync
                         , fftDataR = repeat 0
                         , fftAddrR = repeat 0
                         , dataCntr = 0
                         , lineCounter = 0
                         }

spectrumCtrlT :: SpectrumS -> SpectrumI -> (SpectrumS, SpectrumO)
spectrumCtrlT s@(SpectrumS{..}) inp = (s', outp)
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
    hsyncFalling = hsyncP == 0b1 && hsync == 0b0

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
                  , pixelOut   = encodePixel (last fftDataR)
                  , pixelAddrW = ((resize hsize) * (resize (last fftAddrR))) + (resize fftColCnt)
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
    pixelAddrR' :: Unsigned 19
    pixelAddrR' = (resize counterX) + ((resize lineCounter) * (resize hsize))

    -- Only write pixels on horizontal blank period and when we are purging an fft column buffer
    pixelWrite' = (hsyncP == 0b0) && (stateM == Purge)

    -- Determine pixel addr
    pixelAddr = pack (if pixelWrite then pixelAddrW else pixelAddrR)

    -- Read pixel from SRAM
    pixelIn'                          = sramData
    (pixelRed, pixelGreen, pixelBlue) = decodePixel pixelIn

    -- spectrum is diplayed over entire horizal span
    showSpectrum = counterX > 2 && counterY > 259 && counterY < 512

    (vgaR,vgaG,vgaB)
      | unpack inDisplayArea && showSpectrum
      = (pixelRed,pixelGreen,pixelBlue)
      | otherwise
      = (0,0,0)

encodePixel :: Signed 18 -> BitVector 8
encodePixel val
  | val < 2     && val > (-2)     = 0
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

decodePixel :: BitVector 8 -> (BitVector 4, BitVector 4, BitVector 4)
decodePixel val = (r,g,b)
  where
    (b,g,r) = case val of
      0  -> (0 , 0 , 0 )
      1  -> (0 , 0 , 1 )
      2  -> (0 , 0 , 3 )
      3  -> (0 , 0 , 7 )
      4  -> (0 , 0 , 15)
      5  -> (0 , 1 , 15)
      6  -> (0 , 3 , 15)
      7  -> (0 , 7 , 15)
      8  -> (0 , 15, 15)
      9  -> (1 , 15, 15)
      10 -> (3 , 15, 15)
      11 -> (7 , 15, 15)
      _  -> (15, 15, 15)

