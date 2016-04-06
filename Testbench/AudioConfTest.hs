{-# LANGUAGE RecordWildCards, Arrows #-}
module TestBench.AudioConfTest where

import CLaSH.Prelude
import I2C
import AudioConf
import DE1Types
import Utils
import Debug.Trace
import Numeric
import Data.Char
import qualified Data.Bits as B
import qualified Data.List as L

data ACConfTestS = ACCTS { regFile  :: Vec 16 (Unsigned 8)
                         , addr     :: Vec 8 Bit
                         , cntr     :: Int
                         , atStateM :: AudioTestSM
                         , prevSCL  :: Bit
                         , prevSDA  :: Bit
                         , sdaOut   :: Bit
                         , regAddr  :: Unsigned 8
                         }

data AudioTestSM = ATidle | ATaddr | ATaddrAck | ATreg | ATregAck | ATval | ATvalAck | ATstop

type ACConfTestI = (Bit,Bit)
type ACConfTestO = (Bit,Vec 16 (Unsigned 8))

wolfsonAudioCodecInit :: ACConfTestS
wolfsonAudioCodecInit = ACCTS (replicate d16 0x0) (replicate d8 0) 0 ATidle high high high 0

wolfsonAudioCodecT :: ACConfTestS -> ACConfTestI -> (ACConfTestS, ACConfTestO)
wolfsonAudioCodecT s@(ACCTS{..}) inp = (s', outp)
  where
    (scl,sda) = inp
    outp      = (sdaOut,regFile)

    s'             = stateMachine { prevSDA = prevSDA'
                                  , prevSCL = prevSCL'
                                  }

    prevSDA'       = sda
    prevSCL'       = scl

    startCondition = (prevSDA == high && sda == low) && scl == high
    stopCondition  = (prevSDA == low && sda == high) && scl == high

    sclRising      = prevSCL == low && scl == high
    validAddr      = (v2bv addr) == 0x34  && (length addr) == 8
    validRegAddr   = ((v2bv addr) >= 0 || (v2bv addr) <= 11) && even (v2bv $ reverse addr) && (length addr) == 8

    stateMachine = case atStateM of
      ATidle -> if startCondition then
          s {atStateM = ATaddr}
        else
          s
      ATaddr -> if cntr == 8 then
          if validAddr then
              trace ("Add Rx: Hex: " L.++ ((flip showHex) "" $ toInteger $ v2bv addr) L.++ ", Bin: " L.++ (show $ v2bv addr))  $
              s {atStateM = ATaddrAck, addr = repeat low, cntr = 0}
          else
            trace ("Bad Add Rx: Hex: " L.++ ((flip showHex) "" $ toInteger $ v2bv $ addr) L.++ ", Bin: " L.++ (show $ v2bv addr))  $
            s {atStateM = ATidle, cntr = 0, addr = repeat low}
        else
          if sclRising then
            s {cntr = cntr + 1, addr = addr <<+ sda, sdaOut = high}
          else
            s
      ATaddrAck -> if sclRising then
          s {sdaOut = low, atStateM = ATreg}
        else
          s
      ATreg -> if cntr == 8 then
          if validRegAddr then
              trace ("Reg Rx: Hex: " L.++ ((flip showHex) "" $ toInteger $ v2bv $ addr) L.++ ", Bin: " L.++ (show $ v2bv addr))  $
              s {atStateM = ATregAck, addr = repeat low, cntr = 0, regAddr = (unpack . (\x -> if x == 0 then 0 else x `div` 2) . v2bv) addr}
          else
            trace ("Bad Reg Rx: Hex: " L.++ ((flip showHex) "" $ toInteger $ v2bv $ addr) L.++ ", Bin: " L.++ (show $ v2bv addr))  $
            s {atStateM = ATidle, cntr = 0, addr = repeat low}
        else
          if sclRising then
            s {cntr = cntr + 1, addr = addr <<+ sda, sdaOut = high}
          else
            s
      ATregAck -> if sclRising then
          s {sdaOut = low, atStateM = ATval}
        else
          s
      ATval -> if cntr == 8 then
            trace ("Val Rx: Hex: " L.++ ((flip showHex) "" $ toInteger $ v2bv $ addr) L.++ ", Bin: " L.++ (show $ v2bv addr) L.++ ", regAddr: " L.++ (show regAddr)) $
            s {atStateM = ATvalAck, addr = repeat low, cntr = 0, regFile = replace regAddr (unpack $ v2bv addr) regFile}
        else
          if sclRising then
            s {cntr = cntr + 1, addr = addr <<+ sda, sdaOut = high}
          else
            s
      ATvalAck -> if sclRising then
          s {sdaOut = low, atStateM = ATstop}
        else
          s
      ATstop -> if stopCondition then
          s {atStateM = ATidle, sdaOut = high}
        else
          s

wolfsonAudioCodec = mealyB wolfsonAudioCodecT wolfsonAudioCodecInit

audioConfTest (rst,sdaI) = (done,busy,hostAck,al,ackOut,fault,(sclO,sclOen,sdaO,sdaOen))
  where
    (dout,hostAck,busy,al,ackOut,i2cO) = i2c rst (signal True) 99 start stop (signal False) write (signal False) din (bundle (scl,sdaI))
    (sclO,sclOen,sdaO,sdaOen) = unbundle i2cO
    scl = pack <$> sclOen
    (start,stop,write,din,done,fault) = audioConfig (rst,not <$> rst,hostAck,ackOut,al)

actest rst = (done,fault,map ((flip showHex) "") <$> regFile)
  where
    (done,busy,hostAck,al,ackOut,fault,(sclO,sclOen,sdaO,sdaOen)) = audioConfTest (rst,sdaAud)
    (sdaAud,regFile)                                              = wolfsonAudioCodec (boolToBit <$> sclOen,boolToBit <$> sdaOen)

topEntity rst = (done,fault,regFile)
  where
    (done,busy,hostAck,al,ackOut,fault,(sclO,sclOen,sdaO,sdaOen)) = audioConfTest (rst,sdaAud)
    (sdaAud,regFile) = wolfsonAudioCodec (boolToBit <$> sclOen,boolToBit <$> sdaOen)

testInput :: Signal Bool
testInput = counter .<. 500
  where
    counter :: Signal (Unsigned 18)
    counter = register 0 (counter + 1)

expectedOutput :: (Signal Bool, Signal Bool, Signal (Vec 16 (Unsigned 8)))
               -> Signal Bool
expectedOutput i = assert "actest" (f <$> counter <*> (bundle i)) t (counter .>. 200050)
  where
    counter :: Signal (Unsigned 18)
    counter = register 0 (counter + 1)

    t = mux (counter .<. 500)
            (pure rval)
            (pure fval)

    rval = (False,False,repeat 0)
    fval = (True,False,$(v [0x1f :: Unsigned 8,0x1f,0xf9,0xf9,0x12,0x6,0x0,0x4a,0x1,0x1,0,0,0,0,0,0]))

    f c j
      | c < 10 = rval
      | c > 10 && c < 500 = j
      | c >= 500 && c < 199990 = fval
      | otherwise = j


simulateAcTest :: ((Bool,Bool,Vec 16 String),(Bool,Bool,Vec 16 String))
simulateAcTest = (res L.!! 499, res L.!! 199999)
  where
    res = simulateB actest inp
    inp = L.replicate 500 True L.++ L.replicate 200000 False

bl2i :: [Bit] -> Int
bl2i bv = L.foldl (\a b -> let
                      a' = B.shiftL a 1
                    in
                      if b == high then
                        a' + 1
                      else
                        a'
                 ) 0 bv

b2i :: Bit -> Int
b2i 0b0 = 0
b2i 0b1 = 1

boolToBit :: Bool -> Bit
boolToBit True = high
boolToBit False = low
