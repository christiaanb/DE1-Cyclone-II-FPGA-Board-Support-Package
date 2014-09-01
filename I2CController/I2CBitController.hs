{-# LANGUAGE RecordWildCards #-}
module I2CController.I2CBitController where

import CLaSH.Prelude
import DE1Types
import I2CController.I2CTypes

data BitStateMachine = BITidle |
                       BITstartA | BITstartB | BITstartC | BITstartD | BITstartE |
                       BITstopA | BITstopB | BITstopC | BITstopD |
                       BITrdA | BITrdB | BITrdC | BITrdD |
                       BITwrA | BITwrB | BITwrC | BITwrD
  deriving Eq

data BitCtrlS =
  BitS { bitStateM      :: BitStateMachine -- State Machine
       , cmdAck         :: Bool            -- Command acknowledged register
       , sclOen         :: Bool            -- i2c clock output enable register
       , sdaOen         :: Bool            -- i2c data output enable register
       , sdaChk         :: Bool            -- check SDA statur (multi-master arbitration)
       , dout           :: Bit             -- dout register
       , dsclOen        :: Bool            -- delayed sclOen signal
       , sSCL           :: Bit             -- synchronized SCL input
       , sSDA           :: Bit             -- synchronized SDA input
       , dSCL           :: Bit             -- delayed sSCL
       , dSDA           :: Bit             -- delayed sSDA
       , clkEn          :: Bool            -- statemachine clock enable
       , slaveWait      :: Bool            -- clock generation signal
       , al             :: Bool            -- arbitration lost register
       , cnt            :: Unsigned 16     -- clock devider counter (synthesis)
       , cSCL           :: Vec 2 Bit       -- capture SCL
       , cSDA           :: Vec 2 Bit       -- capture SDA
       , fSCL           :: Vec 3 Bit       -- filter input for SCL
       , fSDA           :: Vec 3 Bit       -- filter input for SDA
       , filterCnt      :: Unsigned 14     -- clock divider for filter
       , startCondition :: Bool            -- start detected
       , stopCondition  :: Bool            -- stop detected
       , cmdStop        :: Bool            -- STOP command
       , busy           :: Bool            -- busy signal
       }

type BitCtrlI = (Bool,Bool,Unsigned 16,BitCtrlSig,I2CIn)
type BitCtrlO = (BitRespSig,Bool,I2COut)

i2cMasterBitCtrlInit =
  BitS { bitStateM      = BITidle     -- State Machine
       , cmdAck         = False       -- Command acknowledged register
       , sclOen         = True        -- i2c clock output enable register
       , sdaOen         = True        -- i2c data output enable register
       , sdaChk         = False       -- check SDA statur (multi-master arbitration)
       , dout           = high        -- dout register
       , dsclOen        = False       -- delayed sclOen signal
       , sSCL           = high        -- synchronized SCL input
       , sSDA           = high        -- synchronized SDA input
       , dSCL           = high        -- delayed sSCL
       , dSDA           = high        -- delayed sSDA
       , clkEn          = True        -- statemachine clock enable
       , slaveWait      = False       -- clock generation signal
       , al             = False       -- arbitration lost register
       , cnt            = 0           -- clock devider counter (synthesis)
       , cSCL           = repeat high -- capture SCL
       , cSDA           = repeat high -- capture SDA
       , fSCL           = repeat high -- filter input for SCL
       , fSDA           = repeat high -- filter input for SDA
       , filterCnt      = 0           -- clock divider for filter
       , startCondition = False       -- start detected
       , stopCondition  = False       -- stop detected
       , cmdStop        = False       -- STOP command
       , busy           = False       -- busy signal
       }

i2cMasterBitCtrlT :: BitCtrlS -> BitCtrlI -> (BitCtrlS, BitCtrlO)
i2cMasterBitCtrlT  s@(BitS {..}) inp = (s', outp)
  where
    -- ==========
    -- = Inputs =
    -- ==========
    -- rst   : synchronous reset, assert high
    -- ena   : core enable signal
    -- clkCnt: clock prescale value
    -- cmd   : command to execute
    -- din   : bit to write to SDA
    -- i2cI  : i2c input lines
    (rst,ena,clkCnt,(cmd,din),i2cI) = inp

    -- ===========
    -- = Outputs =
    -- ===========
    -- cmdAck: command complete
    -- busy  : i2c bus busy
    -- al    : arbitration lost
    -- dout  : bit read from SDA
    -- i2cO  : i2c output lines
    outp = ((cmdAck,al,dout),busy,i2cO)

    -- =============
    -- = i2c lines =
    -- =============
    -- sclI:   i2c clock line input
    -- sclO:   i2c clock line output
    -- sclOen: i2c clock line output enable, active low
    -- sdaI:   i2c data line input
    -- sdaO:   i2c data line output
    -- sdaOen: i2c data line outputs enable, active low
    (sclI,sdaI) = i2cI
    i2cO = (sclO,sclOen,sdaO,sdaOen)

    -- Assign outputs
    sclO = low
    sdaO = low

    -- Update registers
    s' = stateMachine { dout           = dout'           -- dout register
                      , dsclOen        = dsclOen'        -- delayed sclOen signal
                      , sSCL           = sSCL'           -- synchronized SCL input
                      , sSDA           = sSDA'           -- synchronized SDA input
                      , dSCL           = dSCL'           -- delayed sSCL
                      , dSDA           = dSDA'           -- delayed sSDA
                      , clkEn          = clkEn'          -- statemachine clock enable
                      , slaveWait      = slaveWait'      -- clock generation signal
                      , al             = al'             -- arbitration lost register
                      , cnt            = cnt'            -- clock devider counter (synthesis)
                      , cSCL           = cSCL'           -- capture SCL
                      , cSDA           = cSDA'           -- capture SDA
                      , fSCL           = fSCL'           -- filter input for SCL
                      , fSDA           = fSDA'           -- filter input for SDA
                      , filterCnt      = filterCnt'      -- clock divider for filter
                      , startCondition = startCondition' -- start detected
                      , stopCondition  = stopCondition'  -- stop detected
                      , cmdStop        = cmdStop'        -- STOP command
                      , busy           = busy'           -- busy signal
                      }

    -- Whenever the slave is not ready it can delay the cycle by pulling SCL low
    -- delay scloEn
    dsclOen' = sclOen

    -- slaveWait is asserted when the master wants to drive SCL high, but the slave pulls it low
    -- slaveWait remains asserted until the slave releases SCL
    slaveWait' = (sclOen && (not dsclOen) && sSCL == low) || (slaveWait && sSCL == low)

    -- master drives SCL high, but another master pulls it low
    -- master start counting down it low cycle now (clock synchronization)
    sclSync = dSCL == high && sSCL == low && sclOen

    -- generate clk enable signal
    (cnt',clkEn') | rst || cnt == 0 || not ena || sclSync = (clkCnt,True )
                  | slaveWait                             = (cnt   ,False)
                  | otherwise                             = (cnt-1 ,False)

    -- capture SCL and SDA
    (cSCL',cSDA') | rst       = (repeat low   , repeat low   )
                  | otherwise = (cSCL <<+ sclI, cSDA <<+ sdaI)

    -- filter SCL and SDA; (attempt to remove glitches)
    filterCnt' | rst || not ena = 0
               | filterCnt == 0 = resize (shiftR clkCnt 2)
               | otherwise      = filterCnt - 1
    (fSCL',fSDA') | rst            = (repeat high         , repeat high          )
                  | filterCnt == 0 = (fSCL <<+ (head cSCL), fSDA <<+ (head cSDA))
                  | otherwise      = (fSCL                , fSDA                 )

    -- filtered SCL and SDA signals
    (sSCL',sSDA') = if rst then (high,high) else
      ( ((fSCL!!2) .&. (fSCL!!1)) .|.
        ((fSCL!!2) .&. (fSCL!!0)) .|.
        ((fSCL!!1) .&. (fSCL!!0))
      , ((fSDA!!2) .&. (fSDA!!1)) .|.
        ((fSDA!!2) .&. (fSDA!!0)) .|.
        ((fSDA!!1) .&. (fSDA!!0))
      )
    (dSCL',dSDA') = (sSCL,sSDA)

    -- detect start condition => detect falling edge on SDA while SCL is high
    -- detect stop condition  => detect rising edge on SDA wile SCL is high
    (startCondition',stopCondition') = if rst then (False,False) else
      ( (sSDA == low  && dSDA == high) && (sSCL == high)
      , (sSDA == high && dSDA == low ) && (sSCL == high)
      )

    -- i2c busy signal
    busy' = if rst then False else (startCondition || busy) && (not stopCondition)

    -- generate arbitration lost signal
    -- arbitration lost when:
    -- 1) master drives SDA high, but the i2c bus is low
    -- 2) stop detected while not requested (detect during 'idle' state)
    (cmdStop',al') = if rst then
        (False,False)
      else
        ( if clkEn then (if cmd == I2Cstop then True else False) else cmdStop
        , if bitStateM == BITidle then
              (sdaChk && sSDA == low && sdaOen) || (stopCondition && (not cmdStop))
            else
              (sdaChk && sSDA == low && sdaOen)
        )

    -- generate dout signal, store dout on rising edge of SCL
    dout' = if sSCL == high && dSCL == low then sSDA else dout

    -- generate state machine
    sd = s {cmdAck = False}

    stateMachine = if rst || al then
        s {bitStateM = BITidle, cmdAck = False, sclOen = True, sdaOen = True, sdaChk = False}
      else if clkEn then
          case bitStateM of
            -- idle
            BITidle   -> sd { bitStateM = case cmd of
                                  I2Cstart  -> BITstartA
                                  I2Cstop   -> BITstopA
                                  I2Cwrite  -> BITwrA
                                  I2Cread   -> BITrdA
                                  otherwise -> BITidle
                            , sdaChk = False
                            }

            -- start
            BITstartA -> sd { bitStateM = BITstartB
                            , sdaOen   = True   -- set SDA high
                            , sdaChk   = False  -- don't check SDA
                            }
            BITstartB -> sd { bitStateM = BITstartC
                            , sclOen   = True   -- set SCL high
                            , sdaOen   = True   -- keep SDA high
                            , sdaChk   = False  -- don't check SDA
                            }
            BITstartC -> sd { bitStateM = BITstartD
                            , sclOen   = True   -- keep SCL high
                            , sdaOen   = False  -- set SDA low
                            , sdaChk   = False  -- don't check SDA
                            }
            BITstartD -> sd { bitStateM = BITstartE
                            , sclOen   = True   -- keep SCL high
                            , sdaOen   = False  -- keep SDA low
                            , sdaChk   = False  -- don't check SDA
                            }
            BITstartE -> sd { bitStateM = BITidle
                            , cmdAck   = True   -- command completed
                            , sclOen   = False  -- set SCL low
                            , sdaOen   = False  -- keep SDA low
                            , sdaChk   = False  -- don't check SDA
                            }

            -- stop
            BITstopA  -> sd { bitStateM = BITstopB
                            , sclOen   = False  -- keep SCL Low
                            , sdaOen   = False  -- set SDA low
                            , sdaChk   = False  -- don't check SDA
                            }
            BITstopB  -> sd { bitStateM = BITstopC
                            , sclOen   = True   -- set SCL High
                            , sdaOen   = False  -- keep SDA low
                            , sdaChk   = False  -- don't check SDA
                            }
            BITstopC  -> sd { bitStateM = BITstopD
                            , sclOen   = True   -- keep SCL High
                            , sdaOen   = False  -- keep SDA low
                            , sdaChk   = False  -- don't check SDA
                            }
            BITstopD  -> sd { bitStateM = BITidle
                            , cmdAck   = True   -- command completed
                            , sclOen   = True   -- keep SCL High
                            , sdaOen   = True   -- set SDA high
                            , sdaChk   = False  -- don't check SDA
                            }

            -- read
            BITrdA    -> sd { bitStateM = BITrdB
                            , sclOen   = False  -- keep SCL Low
                            , sdaOen   = True   -- tri-state SDA
                            , sdaChk   = False  -- don't check SDA
                            }
            BITrdB    -> sd { bitStateM = BITrdC
                            , sclOen   = True   -- set SCL High
                            , sdaOen   = True   -- tri-state SDA
                            , sdaChk   = False  -- don't check SDA
                            }
            BITrdC    -> sd { bitStateM = BITrdD
                            , sclOen   = True   -- keep SCL High
                            , sdaOen   = True   -- tri-state SDA
                            , sdaChk   = False  -- don't check SDA
                            }
            BITrdD    -> sd { bitStateM = BITidle
                            , cmdAck   = True   -- command completed
                            , sclOen   = False  -- set SCL Low
                            , sdaOen   = True   -- tri-state SDA
                            , sdaChk   = False  -- don't check SDA
                            }

            -- write
            BITwrA    -> sd { bitStateM = BITwrB
                            , sclOen   = False         -- keep SCL Low
                            , sdaOen   = (din == high) -- set SDA
                            , sdaChk   = False         -- don't check SDA (SCL low)
                            }
            BITwrB    -> sd { bitStateM = BITwrC
                            , sclOen   = True          -- set SCL High
                            , sdaOen   = (din == high) -- keep SDA
                            , sdaChk   = False         -- don't check SDA yet
                            }                          -- Allow some more time for SDA and SCL to settle
            BITwrC    -> sd { bitStateM = BITwrD
                            , sclOen   = True          -- keep SCL High
                            , sdaOen   = (din == high) -- keep SDA
                            , sdaChk   = True          -- check SDA
                            }
            BITwrD    -> sd { bitStateM = BITidle
                            , cmdAck   = True          -- command completed
                            , sclOen   = False         -- set SCL Low
                            , sdaOen   = (din == high) -- keep SDA
                            , sdaChk   = False         -- don't check SDA (SCL low)
                            }
        else
          sd

