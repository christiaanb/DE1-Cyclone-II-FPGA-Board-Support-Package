module I2C.Types where

import CLaSH.Prelude
import Control.Lens

data I2Ccommand = I2C_NOP | I2C_Start | I2C_Stop | I2C_Read | I2C_Write
  deriving Eq

data I2CIPair
  = I2CIPair
  { _scli, _sdai :: Bit }

makeLenses ''I2CIPair

data I2COPair
  = I2COPair
  { _sclo   :: Bit
  , _scloEn :: Bool
  , _sdao   :: Bit
  , _sdaoEn :: Bool
  }

makeLenses ''I2COPair
