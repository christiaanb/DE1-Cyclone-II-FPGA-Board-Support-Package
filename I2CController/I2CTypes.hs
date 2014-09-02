module I2CController.I2CTypes where

import CLaSH.Prelude

data I2CCommand = I2Cstart | I2Cstop | I2Cwrite | I2Cread | I2Cnop
  deriving (Eq, Ord)

type BitCtrlSig = (I2CCommand,Bit)
type BitRespSig = (Bool,Bool,Bool,Bit)
