module ToneGeneration.Key2Stepsize (key2stepsize) where

import CLasH.HardwareTypes
import DE1Types

key2stepsize :: Key -> Stepsize
key2stepsize k
  | k == keyTab = toneA
  | k == keyQ   = toneB
  | k == keyW   = toneC
  | k == keyE   = toneD
  | k == keyR   = toneE
  | k == keyT   = toneF
  | k == keyY   = toneG
  | k == keyU   = tonehA
  | k == keyI   = tonehB
  | k == keyO   = tonehC
  | k == keyP   = tonehD
  | k == keyPO  = tonehE
  | k == keyPC  = tonehF
  | k == key1   = toneAis
  | k == key3   = toneCis
  | k == key4   = toneDis
  | k == key6   = toneFis
  | k == key7   = toneGis
  | k == key8   = tonehAis
  | k == key0   = tonehCis
  | k == keyUnd = tonehDis
  | k == keyBsl = tonehFis
  | otherwise   = 0
  where
    keyTab   = 0x0D
    keyQ     = 0x15
    keyW     = 0x1D
    keyE     = 0x24
    keyR     = 0x2D
    keyT     = 0x2C
    keyY     = 0x35
    keyU     = 0x3C
    keyI     = 0x43
    keyO     = 0x44
    keyP     = 0x4D
    keyPO    = 0x54 -- Curly brace open '{'
    keyPC    = 0x5B -- Curly brace close '}'
    key1     = 0x16
    key3     = 0x26
    key4     = 0x25
    key6     = 0x36
    key7     = 0x3D
    key8     = 0x3E
    key0     = 0x45
    keyUnd   = 0x4E -- underscore
    keyBsl   = 0x5D -- backslash 
    toneA    = 601
    toneAis  = 636
    toneB    = 674
    toneC    = 714
    toneCis  = 757
    toneD    = 802
    toneDis  = 850
    toneE    = 900
    toneF    = 954
    toneFis  = 1010
    toneG    = 1070
    toneGis  = 1134
    tonehA   = 1201
    tonehAis = 1273
    tonehB   = 1349
    tonehC   = 1428
    tonehCis = 1513
    tonehD   = 1603
    tonehDis = 1698
    tonehE   = 1800
    tonehF   = 1906
    tonehFis = 2019
