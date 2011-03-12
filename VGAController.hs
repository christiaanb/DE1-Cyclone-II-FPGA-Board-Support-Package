{-# LANGUAGE RecordWildCards #-}
module VGAController (vgaController) where
  
import CLasH.HardwareTypes
import DE1Types

vgaController = comp vgaControllerT vgaInit sysclock

vgaInit :: VGACS
vgaInit = VGACS 0 Low High 0 Low High 0 0

data VGACS = VGACS { hCounter   :: Unsigned D12
                   , vgaBlankHs :: Bit
                   , vgaHs      :: Bit
                   , vCounter   :: Unsigned D12
                   , vgaBlankVs :: Bit
                   , vgaVs      :: Bit
                   , counterX   :: Unsigned D12
                   , counterY   :: Unsigned D12
                   }

type VGACI = (Unsigned D12, Unsigned D12, Unsigned D12, Unsigned D12, Unsigned D12, Unsigned D12, Unsigned D12, Unsigned D12)
type VGACO = ((Bit,Bit),Bit,Unsigned D12, Unsigned D12)

vgaControllerT :: State VGACS -> VGACI -> (State VGACS, VGACO)
vgaControllerT (State s@(VGACS{..})) inp = (State s', outp)
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
      | hCounter == 0                                  = (Low ,High)
      | hCounter == hFrontPorch                        = (Low ,Low )
      | hCounter == (hFrontPorch + hSync)              = (Low ,High)
      | hCounter == (hFrontPorch + hSync + hBackPorch) = (High,High)
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
      | vCounter == 0                                  = (Low ,High)
      | vCounter == vFrontPorch                        = (Low ,Low )
      | vCounter == (vFrontPorch + vSync)              = (Low ,High)
      | vCounter == (vFrontPorch + vSync + vBackPorch) = (High,High)
      | otherwise = (vgaBlankVs,vgaVs)
    
    -- Sync Timing Output
    vgaBlank = vgaBlankVs `hwand` vgaBlankHs

    counterX' = if vgaBlankHs == Low then 0 else counterX + 1
    counterY' = if vgaHsR then
        if vgaBlankVs == Low then 0 else counterY + 1
      else
        counterY
