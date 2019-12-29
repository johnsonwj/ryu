{-|
    This implements the 'basic conversion routine' given in Section 2 of Adams' original paper,
    without using lookup tables to speed up the loops. 
-}

module Text.Format.Floating.Simple where

import Text.Format.Floating.Constants
import Text.Format.Floating.Decimal

import           Data.Text                      ( Text )

formatDec :: (RealFloat a) => a -> Text
formatDec = renderDecimal . convertToDecimal

formatSci :: (RealFloat a) => a -> Text
formatSci = renderDecimalSci . convertToDecimal

-- | Convert a given float to the form (m10, e10) where m10 is the smallest valid integer,
--   such that a = m10 * 10 ^ e10.
convertToDecimal :: (RealFloat a) => a -> DecimalFloat
convertToDecimal x
    | isNaN x       = DecimalNaN mf
    | isInfinite x  = DecimalInfinity s
    | otherwise     = DecimalFloat s d0 (e0 + e10)
  where
    (mf, ef) = decodeFloat x
    s = signum x < 0

    e2 = toInteger ef - 2

    uvw = addIntervals
        (intervalOf $ 4 * mf)
        (FloatInterval (if x == smallestNormalizedFloat then -1 else -2, 0, 2))

    (e10, FloatInterval (a, b, c)) = if e2 >= 0
        then (0, scaleInterval uvw (2 ^ e2))
        else (e2, scaleInterval uvw (5 ^ abs e2))

    (d0, e0) = shortestDecimalRep a b c True False True

newtype FloatInterval = FloatInterval (Integer, Integer, Integer)

scaleInterval :: FloatInterval -> Integer -> FloatInterval
scaleInterval (FloatInterval (a, b, c)) s = FloatInterval (a * s, b * s, c * s)

addIntervals :: FloatInterval -> FloatInterval -> FloatInterval
addIntervals (FloatInterval (a, b, c)) (FloatInterval (a', b', c')) = FloatInterval (a + a', b + b', c + c')

intervalOf :: Integer -> FloatInterval
intervalOf x = FloatInterval (x, x, x)

shortestDecimalRep :: Integer -> Integer -> Integer -> Bool -> Bool -> Bool -> (Integer, Integer)
shortestDecimalRep a0 b0 c0 acceptSmaller acceptLarger breakTieDown
    = go True True a0 b0 (if acceptLarger then c0 else c0 - 1) 0  where
        go az bz a b c i =
            let a'            = a `div` 10
                b'            = b `div` 10
                c'            = c `div` 10
                digit         = b `mod` 10
                adz           = a `mod` 10 == 0
                az'           = az && adz
                bz'           = bz && (digit == 0)
                isTie         = digit == 5 && bz
                wantRoundDown = digit < 5 || (isTie && breakTieDown)
                roundDown     = (wantRoundDown && (a /= b || az)) || b + 1 > c
            in  if a' < c' || (acceptSmaller && az')
                then go az' bz' a' b' c' (i + 1)
                else (if roundDown then b else b + 1, i)
