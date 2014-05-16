module Utils where

import CLaSH.Prelude
import CLaSH.Prelude.Explicit
import Control.Monad.State

import DE1Types

withStateM :: (Pack i, Pack o)
           => s
           -> (i -> State s o)
           -> SignalP i
           -> SignalP o
withStateM initS f = f' <^> initS
  where
    f' = \s i -> let (o,s') = runState (f i) s
                 in  (s',o)

wordSynchronize :: Clock clkIn -> Clock clkOut
                -> a
                -> CSignal clkIn  a
                -> CSignal clkOut a
wordSynchronize clk1 clk2 initS inp =
  delayN clk2 (vcopy d2 initS) (veryUnsafeSynchronizer clk1 clk2 inp)

delayN :: Clock clk
       -> Vec (n + 1) a
       -> CSignal clk a
       -> CSignal clk a
delayN clk initS inp = vlast s
  where
    s = cregisterP clk initS (inp +>> s)

pulseHigh :: Signal Bit -> Signal Bool
pulseHigh = pulseHigh' <^> L
  where
    pulseHigh' inpP inp = (inp,inpP == L && inp == H)

enable a = if a then H else L

hex2display :: Unsigned 4 -> SegDisp
hex2display u = case u of
--            0 ,1 ,2 ,3 ,4 ,5 ,6
  0x0 -> $(v [L ,L ,L ,L ,L ,L ,H ])
  0x1 -> $(v [H ,L ,L ,H ,H ,H ,H ])
  0x2 -> $(v [L ,L ,H ,L ,L ,H ,L ])
  0x3 -> $(v [L ,L ,L ,L ,H ,H ,L ])
  0x4 -> $(v [H ,L ,L ,H ,H ,L ,L ])
  0x5 -> $(v [L ,H ,L ,L ,H ,L ,L ])
  0x6 -> $(v [L ,H ,L ,L ,L ,L ,L ])
  0x7 -> $(v [L ,L ,L ,H ,H ,H ,H ])
  0x8 -> $(v [L ,L ,L ,L ,L ,L ,L ])
  0x9 -> $(v [L ,L ,L ,L ,H ,L ,L ])
  0xA -> $(v [L ,L ,L ,H ,L ,L ,L ])
  0xB -> $(v [H ,H ,L ,L ,L ,L ,L ])
  0xC -> $(v [L ,H ,H ,L ,L ,L ,H ])
  0xD -> $(v [H ,L ,L ,L ,L ,H ,L ])
  0xE -> $(v [L ,H ,H ,L ,L ,L ,L ])
  x   -> $(v [L ,H ,H ,H ,L ,L ,L ])
