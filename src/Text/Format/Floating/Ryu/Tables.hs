{-# LANGUAGE TemplateHaskell #-}

module Text.Format.Floating.Ryu.Tables (floatMultiplier, doubleMultiplier) where

import Text.Format.Floating.Ryu.Tables.TH
import Text.Format.Floating.Constants
import Text.Format.Floating.Ryu.Tables.Formulas

import Data.Array ((!))

makeLookupTable "floatTable" (e2Range (undefined :: Float)) (multiplierTable (undefined :: Float) ryuFloatB0 ryuFloatB1)

makeLookupTable "doubleTable" (e2Range (undefined :: Double)) (multiplierTable (undefined :: Double) ryuDoubleB0 ryuDoubleB1)

floatMultiplier :: Integer -> Integer
floatMultiplier e2 = floatTable ! e2

doubleMultiplier :: Integer -> Integer
doubleMultiplier e2 = doubleTable ! e2
