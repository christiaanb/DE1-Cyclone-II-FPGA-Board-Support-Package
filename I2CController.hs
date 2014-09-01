module I2CController where

import CLaSH.Prelude

import DE1Types
import I2CController.I2CTypes
import I2CController.I2CBitController
import I2CController.I2CByteController

-- topEntity = i2cController

i2cController (rst,ena,clkCnt,start,stop,read,write,ackIn,din,i2cI) =
    (dout,hostAck,busy,al,ackOut,i2cO)
  where
    (hostAck,ackOut,dout,bitCtrl) = (i2cMasterByteCtrlT <^> i2cMasterByteCtrlInit) (rst,start,stop,read,write,ackIn,din,bitResp)
    (bitResp,busy,i2cO)           = (i2cMasterBitCtrlT <^> i2cMasterBitCtrlInit) (rst,ena,clkCnt,bitCtrl,i2cI)
    (cmdAck,al,dbOut)             = sWrap bitResp
