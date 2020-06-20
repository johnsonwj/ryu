{-# LANGUAGE ScopedTypeVariables #-}

module Text.Format.Floating.Constants where

-- | This provides the smallest representable normalized float
--   for a given (IEEE) floating point type. 
smallestNormalizedFloat :: forall a. RealFloat a => a
smallestNormalizedFloat = encodeFloat s e where
    x = undefined :: a
    s = 2 ^ (floatDigits x - 1)
    e = fst (floatRange x) - floatDigits x

-- | These represent the number of bits needed to store the multipliers used by
--   the algorithm; see Section 3.2.4 in Adams' original paper for more information
--   on how they were emperically derived. The values given in Figure 4 of the paper
--   actually differ from what are used in the reference Java implementation; this
--   library is based on the reference implementation, and uses those values
--   (which use the names POW5_INV_BITCOUNT and POW5_BITCOUNT for B0 and B1, respectively)
ryuFloatB0, ryuFloatB1, ryuDoubleB0, ryuDoubleB1 :: Integer
ryuFloatB0 = 59
ryuFloatB1 = 61
ryuDoubleB0 = 125
ryuDoubleB1 = 125
