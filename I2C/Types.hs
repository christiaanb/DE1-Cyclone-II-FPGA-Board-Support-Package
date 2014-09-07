module I2C.Types where

import CLaSH.Prelude
import CLaSH.Signal.Explicit
import CLaSH.Signal.Wrap
import Control.Lens

data I2Ccommand = I2C_NOP | I2C_Start | I2C_Stop | I2C_Read | I2C_Write
  deriving Eq

data I2CIPair
  = I2CIPair
  { _scli, _sdai :: Bit }

makeLenses ''I2CIPair

instance Wrap I2CIPair where
  type Wrapped clk I2CIPair = (CSignal clk Bit, CSignal clk Bit)
  wrap _ i2ci    = (_scli <$> i2ci, _sdai <$> i2ci)
  unwrap _ (a,b) = I2CIPair <$> a <*> b

data I2COPair
  = I2COPair
  { _sclo   :: Bit
  , _scloEn :: Bool
  , _sdao   :: Bit
  , _sdaoEn :: Bool
  }

makeLenses ''I2COPair

instance Wrap I2COPair where
  type Wrapped clk I2COPair = (CSignal clk Bit, CSignal clk Bool, CSignal clk Bit, CSignal clk Bool)
  wrap _ i2co        = (_sclo <$> i2co, _scloEn <$> i2co, _sdao <$> i2co, _sdaoEn <$> i2co)
  unwrap _ (a,b,c,d) = I2COPair <$> a <*> b <*> c <*> d
