{-# LANGUAGE TemplateHaskell #-}
module Text.Format.Floating.Ryu.Tables where

import           Text.Format.Floating.Ryu.Tables.TH
import Text.Format.Floating.Constants
import Text.Format.Floating.Ryu.Tables.Formulas

makeLookupTable "floatTable" (multiplierTable (undefined :: Float) ryuFloatB0 ryuFloatB1)

makeLookupTable "doubleTable" (multiplierTable (undefined :: Double) ryuDoubleB0 ryuDoubleB1)
