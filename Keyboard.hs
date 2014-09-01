module Keyboard where

import CLaSH.Prelude

import Keyboard.CaptureKey
import Keyboard.ConstantKey

topEntity = keyboard

keyboard kbdata = ((dig3,dig2,dig1,dig0),key)
  where
    (dig3,dig2,scancode,byteRead) = captureKey  kbdata
    (dig1,dig0,key)               = constantKey (scancode,byteRead)
