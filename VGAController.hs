{-# LANGUAGE RecordWildCards #-}
module VGAController (vgaController) where

import CLaSH.Prelude
import Control.Lens
import Control.Monad.State

import Utils

data VGACS = VGACS { hCounter   :: Unsigned 12
                   , vgaBlankHs :: Bit
                   , vgaHs      :: Bit
                   , vCounter   :: Unsigned 12
                   , vgaBlankVs :: Bit
                   , vgaVs      :: Bit
                   , counterX   :: Unsigned 12
                   , counterY   :: Unsigned 12
                   }

type VGACI = (Unsigned 12, Unsigned 12, Unsigned 12, Unsigned 12, Unsigned 12, Unsigned 12, Unsigned 12, Unsigned 12)
type VGACO = ((Bit,Bit),Bit,Unsigned 12, Unsigned 12)

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
vgaController = mealyB vgaControllerT vgaInit

vgaControllerT :: VGACS -> VGACI -> (VGACS, VGACO)
vgaControllerT s@(VGACS{..}) inp = (s', outp)
  where
    (hDisp,hFrontPorch,hSync,hBackPorch,vDisp,vFrontPorch,vSync,vBackPorch) = inp
    outp = ((vgaHs,vgaVs),vgaBlank,counterX,counterY)

    s'  = s { hCounter   = hCounter'
            , vgaBlankHs = vgaBlankHs'
            , vgaHs      = vgaHs'
            , vCounter   = vCounter'
            , vgaBlankVs = vgaBlankVs'
            , vgaVs      = vgaVs'
            , counterX   = counterX'
            , counterY   = counterY'
            }

    -- HSync
    hTotal = hDisp + hFrontPorch + hSync + hBackPorch

    hCounter' = if hCounter > (hTotal-1) then 0 else hCounter + 1

    (vgaBlankHs',vgaHs')
      | hCounter == 0                                  = (low ,high)
      | hCounter == hFrontPorch                        = (low ,low )
      | hCounter == (hFrontPorch + hSync)              = (low ,high)
      | hCounter == (hFrontPorch + hSync + hBackPorch) = (high,high)
      | otherwise = (vgaBlankHs,vgaHs)

    -- VSync
    vgaHsR = hCounter == (hFrontPorch + hSync)

    vTotal = vDisp + vFrontPorch + vSync + vBackPorch

    vCounter' = if vgaHsR then
        if vCounter > (vTotal-1) then 0 else vCounter + 1
      else
        vCounter

    (vgaBlankVs',vgaVs') = if vgaHsR then (vgaBlankVs'',vgaVs'') else (vgaBlankVs,vgaVs)
    (vgaBlankVs'',vgaVs'')
      | vCounter == 0                                  = (low ,high)
      | vCounter == vFrontPorch                        = (low ,low )
      | vCounter == (vFrontPorch + vSync)              = (low ,high)
      | vCounter == (vFrontPorch + vSync + vBackPorch) = (high,high)
      | otherwise = (vgaBlankVs,vgaVs)

    -- Sync Timing Output
    vgaBlank = vgaBlankVs .&. vgaBlankHs

    counterX' = if vgaBlankHs == low then 0 else counterX + 1
    counterY' = if vgaHsR then
        if vgaBlankVs == low then 0 else counterY + 1
      else
        counterY
