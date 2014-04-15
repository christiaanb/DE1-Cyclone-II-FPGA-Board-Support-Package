module DE1Types where

import CLaSH.Prelude
import CLaSH.Prelude.Explicit

-- ==========
-- = Clocks =
-- ==========
sysclock :: Clock 1000
sysclock = Clock d1000

bclkClock :: Clock 300
bclkClock = Clock d300

fftClock :: Clock 200
fftClock = Clock d200

-- ================
-- = Type Aliases =
-- ================
type SegDisp     = Vec 7 Bit
type Scancode    = Vec 8 Bit
type Key         = Unsigned 8
type Stepsize    = Signed 16
type I2CIn       = (Bit,Bit)
type I2COut      = (Bit,Bool,Bit,Bool)




