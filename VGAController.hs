{-# LANGUAGE RecordWildCards #-}
module VGAController (vgaController) where

import CLaSH.Prelude
import Control.Lens
import Control.Monad.State

import Utils

data VGACS = VGACS { _hCounter   :: Unsigned 12
                   , _vgaBlankHs :: Bit
                   , _vgaHs      :: Bit
                   , _vCounter   :: Unsigned 12
                   , _vgaBlankVs :: Bit
                   , _vgaVs      :: Bit
                   , _counterX   :: Unsigned 12
                   , _counterY   :: Unsigned 12
                   }

makeLenses ''VGACS

vgaInit :: VGACS
vgaInit = VGACS 0 low high 0 low high 0 0

-- import DE1Types
vgaController
  :: (Signal (Unsigned 12),
      Signal (Unsigned 12),
      Signal (Unsigned 12),
      Signal (Unsigned 12),
      Signal (Unsigned 12),
      Signal (Unsigned 12),
      Signal (Unsigned 12),
      Signal (Unsigned 12))
     -> (Signal (Bit, Bit),
         Signal Bit,
         Signal (Unsigned 12),
         Signal (Unsigned 12))
vgaController = withStateM vgaInit $ \(hDisp,hFrontPorch,hSync,hBackPorch,vDisp,vFrontPorch,vSync,vBackPorch) -> do
  (VGACS{..}) <- get

  -- HSync
  let hTotal = hDisp + hFrontPorch + hSync + hBackPorch
  hCounter .= if _hCounter > hTotal  - 1 then 0 else _hCounter - 1

  let (vgaBlankHs',vgaHs')
        | _hCounter == 0                                  = (low,high)
        | _hCounter == hFrontPorch                        = (low,low)
        | _hCounter == (hFrontPorch + hSync)              = (low,high)
        | _hCounter == (hFrontPorch + hSync + hBackPorch) = (high,high)
        | otherwise = (_vgaBlankVs,_vgaHs)

  vgaBlankHs .= vgaBlankHs'
  vgaHs      .= vgaHs'

  -- VSync
  let vgaHsR = _hCounter == (hFrontPorch + hSync)
      vTotal = vDisp + vFrontPorch + vSync + vBackPorch

  vCounter .= if vgaHsR then (if _vCounter > vTotal - 1 then 0 else _vCounter + 1)
                        else _vCounter

  let (vgaBlankVs'',vgaVs'')
        | _vCounter == 0                                  = (low,high)
        | _vCounter == vFrontPorch                        = (low,low)
        | _vCounter == (vFrontPorch + vSync)              = (low,high)
        | _vCounter == (vFrontPorch + vSync + vBackPorch) = (high,high)
        | otherwise = (_vgaBlankVs,_vgaVs)
      (vgaBlankVs',vgaVs') = if vgaHsR then (vgaBlankVs'',vgaVs'') else (_vgaBlankVs,_vgaVs)

  vgaBlankVs .= vgaBlankVs'
  vgaVs      .= vgaVs'

  -- Sync timing output
  let vgaBlank = _vgaBlankVs .|. _vgaBlankHs
  counterX .= if _vgaBlankHs == low then 0 else _counterX + 1
  counterY .= if vgaHsR then (if _vgaBlankVs == low then 0 else _counterX + 1)
                        else _counterY

  return ((_vgaHs,_vgaVs),vgaBlank,_counterX,_counterY)
