{-# LANGUAGE RecordWildCards #-}
module ToneGeneration.SineGen (sineGen) where

import CLasH.HardwareTypes
import DE1Types

sineGen = comp sineGenT sineGenInit sysclock

data RampS = RS { up    :: Bool 
                , angle :: Signed D16
                , pos   :: Bool
                , enaP  :: Bit
                , stepI :: Signed D16
                }

sineGenInit :: RampS
sineGenInit = RS True 0 True Low 0

sineGenT :: State RampS -> (Bit, Signed D16, Signed D16) -> (State RampS, (Signed D16, Signed D16))
sineGenT (State s@(RS{..})) (ena,sine,step) = (State s', (angle,sine'))
  where
    sine' = if pos then sine else negate sine
    
    enaR = (enaP == Low) && (ena == High)
    
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
