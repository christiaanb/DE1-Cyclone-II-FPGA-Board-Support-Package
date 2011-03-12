{-# LANGUAGE Arrows #-}
module Keyboard where

import CLasH.HardwareTypes

import Keyboard.CaptureKey
import Keyboard.ConstantKey

keyboard = proc kbdata -> do
  (dig3,dig2,scancode,byteRead) <- captureKey  -< kbdata
  (dig1,dig0,key)               <- constantKey -< (scancode,byteRead)
  returnA -< ((dig3,dig2,dig1,dig0),key)