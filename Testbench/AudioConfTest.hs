{-# LANGUAGE RecordWildCards, Arrows #-}
module TestBench.AudioConfTest where

import Prelude hiding ((.))
import qualified Prelude as P
import CLasH.HardwareTypes
import I2CController
import AudioConf
import DE1Types
import Utils
import Debug.Trace
import Numeric
import Data.Char
import qualified Data.Bits as B

data ACConfTestS = ACCTS { regFile  :: [Int]
                         , addr     :: [Bit]
                         , cntr     :: Int
                         , atStateM :: AudioTestSM
                         , prevSCL  :: Bit
                         , prevSDA  :: Bit
                         , sdaOut   :: Bit
                         , regAddr  :: Int
                         }

data AudioTestSM = ATidle | ATaddr | ATaddrAck | ATreg | ATregAck | ATval | ATvalAck | ATstop
                         
type ACConfTestI = (Bit,Bit)
type ACConfTestO = (Bit,[Int])

wolfsonAudioCodecInit :: ACConfTestS
wolfsonAudioCodecInit = ACCTS (replicate 11 0x0) [] 0 ATidle High High High 0

wolfsonAudioCodecT :: State ACConfTestS -> ACConfTestI -> (State ACConfTestS, ACConfTestO)
wolfsonAudioCodecT (State s@(ACCTS{..})) inp = (State s', outp)
  where
    (scl,sda) = inp
    outp      = (sdaOut,regFile)
    
    s'             = stateMachine { prevSDA = prevSDA' 
                                  , prevSCL = prevSCL'
                                  }
    
    prevSDA'       = sda
    prevSCL'       = scl
    
    startCondition = (prevSDA == High && sda == Low) && scl == High
    stopCondition  = (prevSDA == Low && sda == High) && scl == High

    sclRising      = prevSCL == Low && scl == High
    validAddr      = (bl2i $ addr) == 0x34  && (length addr) == 8
    validRegAddr   = ((bl2i addr) >= 0 || (bl2i addr) <= 11) && even (bl2i $ reverse addr) && (length addr) == 8

    stateMachine = case atStateM of
      ATidle -> if startCondition then
          s {atStateM = ATaddr}
        else
          s
      ATaddr -> if cntr == 8 then
          if validAddr then
            -- trace ("Add Rx: Hex: " ++ ((flip showHex) "" $ toInteger $ bl2i $ addr) ++ ", Bin: " ++ (show $ map b2i $ addr))  $ 
              s {atStateM = ATaddrAck, addr = [], cntr = 0}
          else
            trace ("Bad Add Rx: Hex: " ++ ((flip showHex) "" $ toInteger $ bl2i $ addr) ++ ", Bin: " ++ (show $ map b2i $ addr))  $
            s {atStateM = ATidle, cntr = 0, addr = []}
        else
          if sclRising then
            s {cntr = cntr + 1, addr = addr ++ [sda], sdaOut = High}
          else
            s
      ATaddrAck -> if sclRising then
          s {sdaOut = Low, atStateM = ATreg}
        else
          s
      ATreg -> if cntr == 8 then
          if validRegAddr then
            -- trace ("Reg Rx: Hex: " ++ ((flip showHex) "" $ toInteger $ bl2i $ addr) ++ ", Bin: " ++ (show $ map b2i $ addr))  $ 
              s {atStateM = ATregAck, addr = [], cntr = 0, regAddr = ((\x -> if x == 0 then 0 else x `div` 2) P.. bl2i) addr}
          else
            trace ("Bad Reg Rx: Hex: " ++ ((flip showHex) "" $ toInteger $ bl2i $ addr) ++ ", Bin: " ++ (show $ map b2i $ addr))  $ 
            s {atStateM = ATidle, cntr = 0, addr = []}
        else
          if sclRising then
            s {cntr = cntr + 1, addr = addr ++ [sda], sdaOut = High}
          else
            s
      ATregAck -> if sclRising then
          s {sdaOut = Low, atStateM = ATval}
        else
          s
      ATval -> if cntr == 8 then
          -- trace ("Val Rx: Hex: " ++ ((flip showHex) "" $ toInteger $ bl2i $ addr) ++ ", Bin: " ++ (show $ map b2i $ addr))  $
            s {atStateM = ATvalAck, addr = [], cntr = 0, regFile = replace regFile regAddr (bl2i addr)}
        else
          if sclRising then
            s {cntr = cntr + 1, addr = addr ++ [sda], sdaOut = High}
          else
            s
      ATvalAck -> if sclRising then
          s {sdaOut = Low, atStateM = ATstop}
        else
          s
      ATstop -> if stopCondition then
          s {atStateM = ATidle, sdaOut = High}
        else
          s

wolfsonAudioCodec = comp wolfsonAudioCodecT wolfsonAudioCodecInit sysclock

audioConfTest = proc (rst,sdaI) -> do
  rec (dout,hostAck,busy,al,ackOut,i2cO@(sclO,sclOen,sdaO,sdaOen)) <- i2cController   -< (rst,True,99,start,stop,False,write,Low,din,(scl,sdaI))
      scl                                                          <- (arr enable)    -< sclOen
      (start,stop,write,din,done,fault)                            <- audioConfig     -< (rst,not rst,hostAck,ackOut,al)
  returnA -< (done,busy,hostAck,al,ackOut,fault,(sclO,sclOen,sdaO,sdaOen))

actest = proc rst -> do
  rec (done,busy,hostAck,al,ackOut,fault,(sclO,sclOen,sdaO,sdaOen)) <- audioConfTest     -< (rst,sdaAud)
      (sdaAud,regFile)                                              <- wolfsonAudioCodec -< (if sclOen then High else Low, if sdaOen then High else Low)
  returnA -< (done,fault,map ((flip showHex) "") regFile)
  
simulateAcTest = simulateM actest $ do
  setInput True
  run 500
  setInput False
  showOutput
  run 200000
  showOutput

bl2i :: [Bit] -> Int  
bl2i bv = foldl (\a b -> let
                      a' = B.shiftL a 1
                    in
                      if b == High then
                        a' + 1
                      else
                        a'
                 ) 0 bv

b2i Low = 0
b2i High = 1

replace []     _ _ = []
replace (_:xs) 0 y = (y:xs)
replace (x:xs) n y = x : (replace xs (n-1) y) 