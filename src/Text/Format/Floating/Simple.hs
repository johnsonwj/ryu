{-|
    This implements the 'basic conversion routine' given in Section 2 of Adams' original paper,
    without using lookup tables to speed up the loops. 
-}

module Text.Format.Floating.Simple where

import Text.Format.Floating.Constants
import Text.Format.Floating.Decimal
import Text.Format.Floating.Rounding

import Data.Text (Text)

formatDec :: (RealFloat a) => a -> Text
formatDec = formatDec' RoundEven

formatDec' :: (RealFloat a) => RoundingMode -> a -> Text
formatDec' rm x = renderDecimal (convertToDecimal x rm)

formatSci :: (RealFloat a) => a -> Text
formatSci = formatSci' RoundEven

formatSci' :: (RealFloat a) => RoundingMode -> a -> Text
formatSci' rm x = renderDecimalSci (convertToDecimal x rm)

-- | Convert a given float to the form (m10, e10) where m10 is the smallest valid integer,
--   such that a = m10 * 10 ^ e10.
convertToDecimal :: (RealFloat a) => a -> RoundingMode -> DecimalFloat
convertToDecimal x rm
    | isNaN x       = DecimalNaN mf
    | isInfinite x  = DecimalInfinity s
    | otherwise     = DecimalFloat s d0 (e0 + e10)
  where
    (mf, ef) = decodeFloat x
    s = signum x < 0

    e2 = toInteger ef - 2

    uvw = addIntervals
        (intervalOf $ 4 * mf)
        (if x == smallestNormalizedFloat then -1 else -2, 0, 2)

    (e10, (a, b, c)) = if e2 >= 0
        then (0, scaleInterval uvw (2 ^ e2))
        else (e2, scaleInterval uvw (5 ^ abs e2))

    (d0, e0) = shortestDecimalRep a b c (acceptLowerBound rm mf s) (acceptUpperBound rm mf s) True
