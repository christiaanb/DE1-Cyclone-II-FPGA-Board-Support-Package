module Utils where

import CLaSH.Prelude
import CLaSH.Prelude.Explicit
import Control.Monad.State

import DE1Types

withStateM :: (Wrap i, Wrap o)
           => s
           -> (i -> State s o)
           -> SWrapped i
           -> SWrapped o
withStateM initS f = f' <^> initS
  where
    f' = \s i -> let (o,s') = runState (f i) s
                 in  (s',o)

wordSynchronize :: SClock clkIn -> SClock clkOut
                -> a
                -> CSignal clkIn  a
                -> CSignal clkOut a
wordSynchronize clk1 clk2 initS inp =
  delayN clk2 (replicate d2 initS) (veryUnsafeSynchronizer clk1 clk2 inp)

delayN :: KnownNat (n+1)
       => SClock clk
       -> Vec (n + 1) a
       -> CSignal clk a
       -> CSignal clk a
delayN clk initS inp = last s
  where
    s = cregisterW clk initS (inp +>> s)

pulseHigh :: Signal Bit -> Signal Bool
pulseHigh = pulseHigh' <^> low
  where
    pulseHigh' inpP inp = (inp,inpP == low && inp == high)

enable a = if a then high else low

hex2display :: Unsigned 4 -> SegDisp
hex2display u = case u of
--            0,1,2,3,4,5,6
  0x0 -> $$(bLit "0000001")
  0x1 -> $$(bLit "1001111")
  0x2 -> $$(bLit "0010010")
  0x3 -> $$(bLit "0000110")
  0x4 -> $$(bLit "1001100")
  0x5 -> $$(bLit "0100100")
  0x6 -> $$(bLit "0100000")
  0x7 -> $$(bLit "0001111")
  0x8 -> $$(bLit "0000000")
  0x9 -> $$(bLit "0000100")
  0xA -> $$(bLit "0001000")
  0xB -> $$(bLit "1100000")
  0xC -> $$(bLit "0110001")
  0xD -> $$(bLit "1000010")
  0xE -> $$(bLit "0110000")
  x   -> $$(bLit "0111000")
