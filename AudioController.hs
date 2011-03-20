{-# LANGUAGE Arrows, RecordWildCards #-}

module AudioController (audioCtrl) where
  
import CLasH.HardwareTypes
import DE1Types
import Utils

data AudioCtrlS = ACS { lDacCount     :: Index D26
                      , rDacCount     :: Index D26
                      , lAdcCount     :: Index D26
                      , rAdcCount     :: Index D26
                      , pulseAdc48KHz :: Bit
                      , pulseDac48KHz :: Bit
                      , dacData       :: Vector D48 Bit
                      , lAdcData      :: Vector D24 Bit
                      , rAdcData      :: Vector D24 Bit
                      , lAdcDataS     :: Vector D18 Bit
                      , rAdcDataS     :: Vector D18 Bit
                      }
                      
type AudioCtrlI = ((Signed D18, Signed D18, Bool), (Bit,Bit,Bit))
type AudioCtrlO = (Bit,Bit,(Signed D18, Signed D18), Bit)

audioCtrl = proc (ldata,rdata,wEn,clks) -> do
  syncdata <- comp synchronize audioSyncInit bclkClock -< (ldata,rdata,wEn)
  outp     <- comp audioCtrlT audioCtrlInit bclkClock  -< (syncdata,clks)
  returnA -< outp

audioSyncInit = vcopy (0,0,False)

audioCtrlInit :: AudioCtrlS
audioCtrlInit = ACS 0 0 0 0 Low Low (vcopy Low) (vcopy Low) (vcopy Low) (vcopy Low) (vcopy Low)

audioCtrlT :: State AudioCtrlS -> AudioCtrlI -> (State AudioCtrlS, AudioCtrlO)
audioCtrlT (State s@(ACS{..})) inp = (State dacSm, outp)
  where
    ((ldata,rdata,wEn),(adcLRclk,dacLRclk,adcDat)) = inp
    outp = (pulseAdc48KHz,pulseDac48KHz,(bv2s lAdcDataS, bv2s rAdcDataS),vhead dacData)
    
    sd = s { pulseAdc48KHz = Low, pulseDac48KHz = Low }
    
    ldata' = (s2bv ldata) <++> (vcopyn d6 Low)
    rdata' = (s2bv rdata) <++> (vcopyn d6 Low)
    
    adcSm = if adcLRclk == Low then
        let sadcl = sd { lAdcCount = lAdcCount + 1, rAdcCount = 0}
        in
          case lAdcCount of
            0         -> sadcl { pulseAdc48KHz = High, lAdcDataS = vtake d18 lAdcData, rAdcDataS = vtake d18 rAdcData}
            25        -> sd
            otherwise -> sadcl { lAdcData = lAdcData <<+ adcDat }            
      else -- adcLRclk == High
        let sadcr = s { rAdcCount = rAdcCount + 1, lAdcCount = 0 }
        in
          case rAdcCount of
            0         -> sadcr
            25        -> sd
            otherwise -> sadcr { rAdcData = rAdcData <<+ adcDat }
            
    dacSm = if dacLRclk == Low then
        let sdacl = adcSm { lDacCount = lDacCount + 1, rDacCount = 0 }
        in
          case lDacCount of
            0         -> sdacl { dacData = ldata' <++> rdata', pulseDac48KHz = High }
            25        -> adcSm
            otherwise -> sdacl { dacData = dacData <<+ Low }
      else -- dacLRclk == High
        let sdacr = adcSm { rDacCount = rDacCount + 1, lDacCount = 0 }
        in
          case rDacCount of
            0         -> sdacr
            25        -> adcSm
            otherwise -> sdacr { dacData = dacData <<+ Low }
