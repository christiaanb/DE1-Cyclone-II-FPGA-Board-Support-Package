{-# LANGUAGE Arrows #-}
module ToneGeneration (toneGeneration) where

import CLasH.HardwareTypes
import DE1Types
import Utils
import ToneGeneration.Cordic
import ToneGeneration.SineGen
import ToneGeneration.Key2Stepsize

toneGeneration :: Comp (Bit, Key) (Signed D16)
toneGeneration = proc (ena,key) -> do
  rec enaS          <- (comp synchronize syncEnableInit sysclock) -< ena
      (angle,sine') <- sineGen                                    -< (enaS,sine,step)
      step          <- (arr key2stepsize)                         -< key
      (cosine,sine) <- cordic                                     -< angle
  returnA -< sine'

syncEnableInit = vcopy Low
