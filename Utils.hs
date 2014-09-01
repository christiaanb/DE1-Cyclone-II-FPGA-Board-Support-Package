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

hex2display :: BitVector 4 -> SegDisp
hex2display u = case u of
--                6543210
  0x0 -> $$(bLit "1000000")
  0x1 -> $$(bLit "1111001")
  0x2 -> $$(bLit "0100100")
  0x3 -> $$(bLit "0110000")
  0x4 -> $$(bLit "0011001")
  0x5 -> $$(bLit "0010010")
  0x6 -> $$(bLit "0000010")
  0x7 -> $$(bLit "1111000")
  0x8 -> $$(bLit "0000000")
  0x9 -> $$(bLit "0010000")
  0xA -> $$(bLit "0001000")
  0xB -> $$(bLit "0000011")
  0xC -> $$(bLit "1000110")
  0xD -> $$(bLit "0100001")
  0xE -> $$(bLit "0000110")
  f   -> $$(bLit "0001110")
