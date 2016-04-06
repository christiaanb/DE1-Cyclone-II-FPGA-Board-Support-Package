module SRAMcontroller (sramController) where

import CLaSH.Prelude

type SramI = (BitVector 16, (BitVector 8 , BitVector 19, Bool))
type SramO = (BitVector 8 , (BitVector 16, BitVector 18, Bit, Bit, Bit, Bit, Bit))

sramController :: Signal SramI -> Signal SramO
sramController = fmap sramControllerT

sramControllerT :: SramI -> SramO
sramControllerT (sramIn,(dataIn,address,write)) =
  (dataOut,(sramOut,slice d18 d1 address,weN,oeN,ubN,lbN,ceN))
  where
    ulByte = lsb address

    ubN = ulByte
    lbN = complement ulByte

    ceN = 0b0

    weN = if write then 0b0 else 0b1
    oeN = if write then 0b1 else 0b0

    dataOut = if write then 0 else
      if ulByte == 0b0 then slice d15 d8 sramIn
                       else slice d7  d0 sramIn

    sramOut = if ulByte == 0b0 then dataIn ++# (0 :: BitVector 8)
                               else (0 :: BitVector 8) ++# dataIn

