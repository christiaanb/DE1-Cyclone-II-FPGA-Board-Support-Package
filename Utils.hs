{-# LANGUAGE TemplateHaskell #-}

module Utils where

import CLasH.HardwareTypes
import DE1Types

synchronize :: State (Vector D2 a) -> a -> (State (Vector D2 a), a)
synchronize (State s) inp = (State s', outp)
  where
    s'   = inp +>> s
    outp = vlast s
 
delayN (State s) inp = (State (inp +>> s), vlast s)

pulseHigh :: State Bit -> Bit -> (State Bit, Bool)
pulseHigh (State inpP) inp = (State inp, (inpP == Low) && (inp == High))   

enable a = if a then High else Low

hex2display :: Unsigned D4 -> SegDisp
hex2display v = case v of
--              0   ,1   ,2   ,3   ,4   ,5   ,6
  0x0 -> $(vTH [Low ,Low ,Low ,Low ,Low ,Low ,High])
  0x1 -> $(vTH [High,Low ,Low ,High,High,High,High])
  0x2 -> $(vTH [Low ,Low ,High,Low ,Low ,High,Low ])
  0x3 -> $(vTH [Low ,Low ,Low ,Low ,High,High,Low ])
  0x4 -> $(vTH [High,Low ,Low ,High,High,Low ,Low ])
  0x5 -> $(vTH [Low ,High,Low ,Low ,High,Low ,Low ])
  0x6 -> $(vTH [Low ,High,Low ,Low ,Low ,Low, Low ])
  0x7 -> $(vTH [Low ,Low ,Low ,High,High,High,High])
  0x8 -> $(vTH [Low ,Low ,Low ,Low ,Low ,Low ,Low ])
  0x9 -> $(vTH [Low ,Low ,Low ,Low ,High,Low ,Low ])
  0xA -> $(vTH [Low ,Low ,Low ,High,Low ,Low ,Low ])
  0xB -> $(vTH [High,High,Low ,Low ,Low ,Low ,Low ])
  0xC -> $(vTH [Low ,High,High,Low ,Low ,Low ,High])
  0xD -> $(vTH [High,Low ,Low ,Low ,Low ,High,Low ])
  0xE -> $(vTH [Low ,High,High,Low ,Low ,Low ,Low ])
  x   -> $(vTH [Low ,High,High,High,Low ,Low ,Low ])
