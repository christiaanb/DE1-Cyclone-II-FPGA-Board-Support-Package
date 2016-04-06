module Utils where

import CLaSH.Prelude
import CLaSH.Prelude.Explicit
import Control.Monad.State

import DE1Types

wordSynchronize :: SClock clkIn -> SClock clkOut
                -> a
                -> Signal' clkIn  a
                -> Signal' clkOut a
wordSynchronize clk1 clk2 initS inp =
  delayN clk2 (replicate d2 initS) (unsafeSynchronizer clk1 clk2 inp)

delayN :: KnownNat (n+1)
       => SClock clk
       -> Vec (n + 1) a
       -> Signal' clk a
       -> Signal' clk a
delayN clk initS inp = last s
  where
    s = registerB' clk initS (inp +>> s)

hex2display :: BitVector 4 -> SegDisp
hex2display u = case u of
--                6543210
  0x0 -> 0b1000000
  0x1 -> 0b1111001
  0x2 -> 0b0100100
  0x3 -> 0b0110000
  0x4 -> 0b0011001
  0x5 -> 0b0010010
  0x6 -> 0b0000010
  0x7 -> 0b1111000
  0x8 -> 0b0000000
  0x9 -> 0b0010000
  0xA -> 0b0001000
  0xB -> 0b0000011
  0xC -> 0b1000110
  0xD -> 0b0100001
  0xE -> 0b0000110
  f   -> 0b0001110

instance Bundle (a,b,c,d,e,f,g,h,i,j,k,l,m) where
  type Unbundled' t (a,b,c,d,e,f,g,h,i,j,k,l,m) =
    ( Signal' t a, Signal' t b, Signal' t c, Signal' t d, Signal' t e
    , Signal' t f, Signal' t g, Signal' t h, Signal' t i, Signal' t j
    , Signal' t k, Signal' t l, Signal' t m
    )
  bundle'   _ (a,b,c,d,e,f,g,h,i,j,k,l,m) =
    (,,,,,,,,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h <*> i <*> j
                   <*> k <*> l <*> m
  unbundle' _ tup               = (fmap (\(x,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                  ,fmap (\(_,x,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                  ,fmap (\(_,_,x,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                  ,fmap (\(_,_,_,x,_,_,_,_,_,_,_,_,_) -> x) tup
                                  ,fmap (\(_,_,_,_,x,_,_,_,_,_,_,_,_) -> x) tup
                                  ,fmap (\(_,_,_,_,_,x,_,_,_,_,_,_,_) -> x) tup
                                  ,fmap (\(_,_,_,_,_,_,x,_,_,_,_,_,_) -> x) tup
                                  ,fmap (\(_,_,_,_,_,_,_,x,_,_,_,_,_) -> x) tup
                                  ,fmap (\(_,_,_,_,_,_,_,_,x,_,_,_,_) -> x) tup
                                  ,fmap (\(_,_,_,_,_,_,_,_,_,x,_,_,_) -> x) tup
                                  ,fmap (\(_,_,_,_,_,_,_,_,_,_,x,_,_) -> x) tup
                                  ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,x,_) -> x) tup
                                  ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,x) -> x) tup
                                  )
