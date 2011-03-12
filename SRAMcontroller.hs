module SRAMcontroller (sramController) where

import CLasH.HardwareTypes

type SramI = (Vector D16 Bit, (Vector D8 Bit, Vector D19 Bit, Bool))
type SramO = (Vector D8 Bit, (Vector D16 Bit, Vector D18 Bit, Bit, Bit, Bit, Bit, Bit))

sramController :: Comp SramI SramO
sramController = arr sramControllerT

sramControllerT :: SramI -> SramO
sramControllerT (sramIn,(dataIn,address,write)) = 
  (vreverse dataOut,(vreverse sramOut, vreverse (vinit address),weN,oeN,ubN,lbN,ceN))
  where
    ulByte = vlast address
    
    ubN = ulByte
    lbN = hwnot ulByte
    
    ceN = Low
    
    weN = if write then Low  else High
    oeN = if write then High else Low
    
    dataOut = if write then (vcopy Low) else
      if ulByte == Low then vtake d8 sramIn
                       else vdrop d8 sramIn
    
    sramOut = if ulByte == Low then dataIn <++> (vcopyn d8 Low)
                               else (vcopyn d8 Low) <++> dataIn

