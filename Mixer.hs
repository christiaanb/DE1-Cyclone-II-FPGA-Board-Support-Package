module Mixer where

import CLaSH.Prelude

mixer :: ( (Signal (Signed 18), Signal (Signed 16))
         , (Signal (Signed 18), Signal (Signed 16)))
      -> (Signal (Signed 18), Signal (Signed 18))
mixer (l,r) = unpack (mixerT <$> pack (pack l, pack r))

mixerT :: ((Signed 18, Signed 16), (Signed 18, Signed 16)) -> (Signed 18, Signed 18)
mixerT ((lChannel1,lChannel2),(rChannel1,rChannel2)) = (lChannelOut,rChannelOut)
  where
    lChannel2'  = fromBV ((toBV lChannel2) <++> (vcopy d2 L))
    rChannel2'  = fromBV ((toBV rChannel2) <++> (vcopy d2 L))
    lChannelOut = (shiftR lChannel1 1) + (shiftR lChannel2' 1)
    rChannelOut = (shiftR rChannel1 1) + (shiftR rChannel2' 1)
