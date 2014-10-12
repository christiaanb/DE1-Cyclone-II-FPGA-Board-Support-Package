module DE1Types
  ( module DE1Types
  , SystemClock
  , systemClock
  )
where

import CLaSH.Prelude
import CLaSH.Prelude.Explicit

-- ==========
-- = Clocks =
-- ==========

-- Already defined in "CLaSH.Prelude.Explicit"
--
-- type SystemClock = Clk "system" 1000
-- systemClock :: SClock SystemClock

type BClkClock = Clk "bclk" 3000

bclkClock :: SClock BClkClock
bclkClock = sclock

type FFTClock = Clk "fft" 2000

fftClock :: SClock FFTClock
fftClock = sclock

type KBClock = Clk "keyboard" 8000

kbClock :: SClock KBClock
kbClock = sclock

-- ================
-- = Type Aliases =
-- ================
type SegDisp     = BitVector 7
type Scancode    = BitVector 8
type Stepsize    = Signed 16
type I2CIn       = (Bit,Bit)
type I2COut      = (Bit,Bool,Bit,Bool)

type Word = Signed 18
type DWord = Signed 36
type CWord = (Word, Word)
type FPConst = (Word, Word)
type CFPConst = (FPConst, FPConst)


