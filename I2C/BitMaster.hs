{-# LANGUAGE RecordWildCards #-}
module I2C.BitMaster where

import CLaSH.Prelude

import Control.Lens
import Control.Monad
import Control.Monad.Trans.State
import Data.Tuple

import I2C.BitMaster.BusCtrl
import I2C.BitMaster.StateMachine
import I2C.Types

data BitMasterS
  = BitS
  { _busState       :: BusStatusCtrl
  , _stateMachine   :: StateMachine
  , _dout           :: Bit             -- dout register
  , _dsclOen        :: Bool            -- delayed sclOen signal
  , _clkEn          :: Bool            -- statemachine clock enable
  , _slaveWait      :: Bool            -- clock generation signal
  , _cnt            :: Unsigned 16     -- clock devider counter (synthesis)
  }

makeLenses ''BitMasterS

type BitMasterI = (Bool,Bool,Unsigned 16,BitCtrlSig,I2CIn)
type BitMasterO = (BitRespSig,Bool,I2COut)

bitMasterInit = BitS { _stateMachine   = stateMachineStart
                            , _busState       = busStartState
                            , _dout           = high       -- dout register
                            , _dsclOen        = False      -- delayed sclOen signal
                            , _clkEn          = True       -- statemachine clock enable
                            , _slaveWait      = False      -- clock generation signal
                            , _cnt            = 0          -- clock devider counter (synthesis)
                            }

{-# NOINLINE bitMasterT #-}
bitMasterT :: BitMasterS -> BitMasterI -> (BitMasterS, BitMasterO)
bitMasterT s@(BitS { _stateMachine = StateMachine  {..}
                   , _busState     = BusStatusCtrl {..}
                   , ..
                   })
                  (rst,ena,clkCnt,(cmd,din),i2cI@(sclI,sdaI)) = swap $ flip runState s $ do
  -- Whenever the slave is not ready it can delay the cycle by pulling SCL low
  -- delay scloEn
  dsclOen .= _sclOen

  -- slaveWait is asserted when the master wants to drive SCL high, but the slave pulls it low
  -- slaveWait remains asserted until the slave releases SCL
  let masterSclHigh = _sclOen && not _dsclOen
      (sSCL,sSDA)   = _sI2C
  slaveWait .= ((masterSclHigh || _slaveWait) && sSCL == 0)

  -- master drives SCL high, but another master pulls it low
  -- master start counting down it low cycle now (clock synchronization)
  let dSCL    = fst _dI2C
      sclSync = dSCL == high && sSCL == low && _sclOen

  -- generate clk enable signal
  if rst || _cnt == 0 || not ena || sclSync then do
     cnt   .= clkCnt
     clkEn .= True
  else if _slaveWait then do
     clkEn .= False
  else do
     cnt   -= 1
     clkEn .= False

  -- bus status controller
  zoom busState (busStatusCtrl rst ena clkCnt cmd _clkEn i2cI _bitStateM _sdaChk _sdaOen)

  -- generate dout signal, store dout on rising edge of SCL
  when (sSCL == high && dSCL == low) $
    dout .= sSDA

  -- state machine
  zoom stateMachine (bitStateMachine rst _al _clkEn cmd din)

  -- assign outputs
  let sclO = low
      sdaO = low
      i2cO = (sclO,_sclOen,sdaO,_sdaOen)
      outp = ((_cmdAck,_al,_dout),_busy,i2cO)

  return outp
