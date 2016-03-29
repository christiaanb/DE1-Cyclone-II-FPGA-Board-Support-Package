module Mixer where

import CLaSH.Prelude
import CLaSH.Prelude.Explicit

mixer :: ( (Signal' clk (Signed 18), Signal' clk (Signed 16))
         , (Signal' clk (Signed 18), Signal' clk (Signed 16)))
      -> (Signal' clk (Signed 18), Signal' clk (Signed 18))
mixer ((lChannel1,lChannel2),(rChannel1,rChannel2)) = (lChannelOut,rChannelOut)
  where
    lChannel2'  = shiftL <$> (resize <$> lChannel2) <*> 1
    rChannel2'  = shiftL <$> (resize <$> rChannel2) <*> 1
    lChannelOut = (shiftR <$> lChannel1 <*> 1) + lChannel2'
    rChannelOut = (shiftR <$> rChannel1 <*> 1) + rChannel2'
