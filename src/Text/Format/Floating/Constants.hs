{-# LANGUAGE ScopedTypeVariables #-}

module Text.Format.Floating.Constants where

smallestNormalizedFloat :: forall a. RealFloat a => a
smallestNormalizedFloat = encodeFloat s e where
    x = undefined :: a
    s = 2 ^ (floatDigits x - 1)
    e = fst (floatRange x) - floatDigits x

ryuFloatB0, ryuFloatB1, ryuDoubleB0, ryuDoubleB1 :: Integer
ryuFloatB0 = 59
ryuFloatB1 = 61
ryuDoubleB0 = 122
ryuDoubleB1 = 121
