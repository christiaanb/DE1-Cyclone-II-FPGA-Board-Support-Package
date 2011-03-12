{-# LANGUAGE Arrows #-}
module I2CController where

import CLasH.HardwareTypes
import qualified Prelude as P
import DE1Types
import I2CController.I2CTypes
import I2CController.I2CBitController
import I2CController.I2CByteController

i2cController = proc (rst,ena,clkCnt,start,stop,read,write,ackIn,din,i2cI) -> do
  rec (hostAck,ackOut,dout,bitCtrl)           <- (comp i2cMasterByteCtrlT i2cMasterByteCtrlInit sysclock) -< (rst,start,stop,read,write,ackIn,din,bitResp)
      (bitResp@((cmdAck,al,dbout)),busy,i2cO) <- (comp i2cMasterBitCtrlT  i2cMasterBitCtrlInit sysclock)  -< (rst,ena,clkCnt,bitCtrl,i2cI)
  returnA -< (dout,hostAck,busy,al,ackOut,i2cO)