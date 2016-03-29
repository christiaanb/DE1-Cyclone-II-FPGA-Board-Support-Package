{-# LANGUAGE RecordWildCards #-}
module ToneGeneration.SineGen (sineGen) where

import CLaSH.Prelude
import DE1Types

sineGen = mealyB sineGenT sineGenInit

data RampS = RS { up    :: Bool
                , angle :: Signed 16
                , pos   :: Bool
                , enaP  :: Bit
                , stepI :: Signed 16
                }

sineGenInit :: RampS
sineGenInit = RS True 0 True 0b0 0

sineGenT :: RampS -> (Bit, Signed 16, Signed 16) -> (RampS, (Signed 16, Signed 16))
sineGenT s@(RS{..}) (ena,sine,step) = (s', (angle,sine'))
  where
    sine' = if pos then sine else negate sine

    enaR = (enaP == 0b0) && (ena == 0b1)

    s' = if enaR then sm {stepI = step} else sd

    sd = s {enaP = ena}

    sm = if stepI == 0 then
          sd {angle = 0, up = True}
      else if up then
        if (angle+stepI) > 16384 then
          sd {angle = angle - stepI, up = False}
        else
          sd {angle = angle + stepI}
      else
        if (angle-stepI) < 0 then
          sd {angle = angle + step, up = True, pos = not pos}
        else
          sd {angle = angle - stepI}
