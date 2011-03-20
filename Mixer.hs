module Mixer where

import CLasH.HardwareTypes

mixer :: Comp ((Signed D18, Signed D16), (Signed D18, Signed D16)) (Signed D18, Signed D18)  
mixer = arr mixerT

mixerT :: ((Signed D18, Signed D16), (Signed D18, Signed D16)) -> (Signed D18, Signed D18)  
mixerT ((lChannel1,lChannel2),(rChannel1,rChannel3)) = (lChannelOut,rChannelOut)
  where
    lChannel2'  = bv2s ((s2bv lChannel2) <++> (vcopyn d2 Low))
    rChannel2'  = bv2s ((s2bv lChannel2) <++> (vcopyn d2 Low))
    lChannelOut = (shiftR lChannel1 1) + (shiftR lChannel2' 1)
    rChannelOut = (shiftR rChannel1 1) + (shiftR rChannel2' 1)