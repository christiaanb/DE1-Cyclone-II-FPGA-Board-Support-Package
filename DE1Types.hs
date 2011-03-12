module DE1Types where

import CLasH.HardwareTypes

-- ==========
-- = Clocks =
-- ==========
kbclock :: Clock
kbclock = ClockDown 20

sysclock :: Clock
sysclock = ClockUp 1

bclkClock :: Clock
bclkClock = ClockUp 3

fftClock :: Clock
fftClock = ClockUp 2

-- ================
-- = Type Aliases =
-- ================
type SegDisp     = Vector D7 Bit
type Scancode    = Vector D8 Bit
type Key         = Unsigned D8
type Stepsize    = Signed D16
type I2CIn       = (Bit,Bit)
type I2COut      = (Bit,Bool,Bit,Bool)




