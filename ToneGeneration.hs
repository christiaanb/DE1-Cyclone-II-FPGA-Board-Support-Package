module ToneGeneration (toneGeneration) where

import CLaSH.Prelude
import CLaSH.Prelude.Explicit
import DE1Types
import Utils
import ToneGeneration.Cordic
import ToneGeneration.SineGen
import ToneGeneration.Key2Stepsize

toneGeneration :: Signal' BClkClock Bit
               -> Signal Scancode
               -> Signal (Signed 16)
toneGeneration ena key = sine'
  where
    enaS = wordSynchronize bclkClock systemClock 0b0 ena
    (angle,sine') = sineGen (enaS,sine,step)
    step = key2stepsize <$> key
    (cosine,sine) = cordic angle
