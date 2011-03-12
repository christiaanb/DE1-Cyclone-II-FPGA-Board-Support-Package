{-# LANGUAGE Arrows, ScopedTypeVariables, TemplateHaskell #-}
module ToneGeneration.Cordic (cordic) where
  
import CLasH.HardwareTypes
import DE1Types

type CordicS = Vector D16 (Signed D16, Signed D16, Signed D20)
type CordicI = Signed D16
type CordicO = (Signed D16, Signed D16)

cordic = proc inp -> do
  outp <- (comp cordicCoreT cordicCoreInit sysclock) -< inp
  returnA -< outp

cordicCoreInit :: CordicS
cordicCoreInit = vcopy (0,0,0)

cordicCoreT :: (State CordicS) -> CordicI -> (State CordicS, CordicO)
cordicCoreT (State s) zi = (State s', (cosine,sine))
  where
    (cosine,sine,zero) = vlast s
    
    x0 = 0x4DBA
    y0 = 0
    z0 = shiftL (resizeSigned zi) 4
    
    ids = $(vTH ([0..15]::[Signed D16]))
    pipeIns = vzip ids ((x0,y0,z0) +>> s)
    
    s' = vmap cordicPipe pipeIns

cordicPipe (pipeId,(xi,yi,zi)) = (xo,yo,zo)
  where
    dx   = shiftR xi pipeId
    dy   = shiftR yi pipeId
    atan = catan pipeId
    zNeg = vhead (s2bv zi)
    zPos = hwnot zNeg
    xo   = addSub zNeg xi dy
    yo   = addSub zPos yi dx
    zo   = addSub zNeg zi atan

-- | Function catan (constant arc-tangent)
-- Lookup table containing pre-calculated arc-tangents.
-- The numbers are calculated as follows: Z(n) = atan(1/2^n)
-- examples:
-- 20 bit values => 2^20 = 2pi(rad)
--                  1(rad) = 2^20/2pi = 166886,054
-- n:0, atan(1/1) = 0.7854..(rad)
--      0.7853.. * 166886.054 = 131072 (dec) = 20000 (hex)
-- n:1, atan(1/2) = 0.4636..(rad)
--      0.4636.. * 166886.054 = 77376.32 (dec) = 12E40 (hex)
-- n:2  atan(1/4) = 0.2449..(rad)
--      0.2449.. * 166886.054 = 40883.52 (dec) = 9FB4 (hex)
-- n:3  atan(1/8) = 0.1243..(rad)
--      0.1243.. * 166886.054 = 20753.11 (dec) = 5111 (hex)
-- 
catan :: Signed D16 -> Signed D20
catan n = case n of
  0  -> 0x020000
  1  -> 0x012E40
  2  -> 0x09FB4
  3  -> 0x05111
  4  -> 0x028B1
  5  -> 0x0145D
  6  -> 0x0A2F
  7  -> 0x0518
  8  -> 0x028C
  9  -> 0x0146
  10 -> 0x0A3
  11 -> 0x051
  12 -> 0x029
  13 -> 0x014
  14 -> 0x0A
  15 -> 0x05
  16 -> 0x03
  17 -> 0x01
  x  -> 0x0
  
addSub High a b = a + b
addSub Low  a b = a - b