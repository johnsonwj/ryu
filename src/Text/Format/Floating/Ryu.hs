module Text.Format.Floating.Ryu where

import Text.Format.Floating.Constants
import Text.Format.Floating.Decimal
import Text.Format.Floating.Rounding
import Text.Format.Floating.Ryu.Tables
import qualified Text.Format.Floating.Ryu.Tables.Formulas as Formulas

import Data.Text (Text)

formatDec :: (RyuFloat a) => a -> Text
formatDec = formatDec' RoundEven

formatDec' :: (RyuFloat a) => RoundingMode -> a -> Text
formatDec' rm x = renderDecimal (convertToDecimal x rm)

-- | This always uses a lowercase @e@ in the output. Compose with 'Data.Text.toUpper'
--   if uppercase output is desired.
formatSci :: (RyuFloat a) => a -> Text
formatSci = formatSci' RoundEven

formatSci' :: (RyuFloat a) => RoundingMode -> a -> Text
formatSci' rm x = renderDecimalSci (convertToDecimal x rm)

class RealFloat a => RyuFloat a where
  ryuE2 :: a -> Integer
  ryuE2 x = toInteger $ snd (decodeFloat x) - 2

  ryuQ :: a -> Integer
  ryuQ = Formulas.ryuQ . ryuE2

  ryuK :: a -> Integer
  ryuK x
    | e2 >= 0   = Formulas.ryuK0 e2 b0
    | otherwise = Formulas.ryuK1 e2 b1
    where
      e2 = ryuE2 x
      b0 = ryuB0 x
      b1 = ryuB1 x

  -- | 'ryuB0' and 'ryuB1' do not evaluate their argument.
  ryuB0 :: a -> Integer
  ryuB1 :: a -> Integer

  ryuMultiplier :: a -> Integer

instance RyuFloat Float where
  ryuMultiplier = floatMultiplier . ryuE2

  ryuB0 _ = ryuFloatB0
  ryuB1 _ = ryuFloatB1

instance RyuFloat Double where
  ryuMultiplier = doubleMultiplier . ryuE2

  ryuB0 _ = ryuDoubleB0
  ryuB1 _ = ryuDoubleB1

convertToDecimal :: RyuFloat a => a -> RoundingMode -> DecimalFloat
convertToDecimal x rm
    | isNaN x           = DecimalNaN mf
    | isInfinite x      = DecimalInfinity s
    | x == 0.0          = DecimalFloat False 0 0
    | isNegativeZero x  = DecimalFloat True 0 0
    | otherwise         = DecimalFloat s (e0 + e10) d0
  where
    mf = fst $ decodeFloat x
    e2 = ryuE2 x
    q = ryuQ x
    k = ryuK x
    s = signum x < 0

    uvw = addIntervals
        (intervalOf $ 4 * mf)
        (if x == smallestNormalizedFloat then -1 else -2, 0, 2)
    (u, v, w) = uvw

    (e10, shift, zbase) = if e2 >= 0 then (0, -e2 + q + k, 5) else (e2, q - k, 2)

    (a, b, c) = shiftIntervalRight (scaleInterval uvw (ryuMultiplier x)) (fromInteger shift)
    za = u `mod` (zbase ^ q) == 0
    zb = v `mod` (zbase ^ (q - 1)) == 0
    zc = w `mod` (zbase ^ q) == 0

    acceptSmaller = (acceptLowerBound rm mf s) && za
    acceptLarger = (acceptUpperBound rm mf s) || (not zc)

    (d0, e00) = shortestDecimalRep a b c acceptSmaller zb acceptLarger True
    e0 = e00 + q

shortestDecimalRep :: Integer -> Integer -> Integer -> Bool -> Bool -> Bool -> Bool -> (Integer, Integer)
shortestDecimalRep a0 b0 c0 acceptSmaller bz0 acceptLarger breakTieDown
    = loop True bz0 a0 b0 (if acceptLarger then c0 else c0 - 1) 0 0  where
        loop az bz a b c digit i =
            let a'            = a `div` 10
                b'            = b `div` 10
                c'            = c `div` 10
                digit'        = b `mod` 10
                adz           = a `mod` 10 == 0
                az'           = az && adz
                bz'           = bz && (digit == 0)
                isTie         = digit == 5 && bz
                wantRoundDown = digit < 5 || (isTie && breakTieDown)
                roundDown     = (wantRoundDown && (a /= b || az)) || b + 1 > c
            in  if a' < c' || (acceptSmaller && az')
                then loop az' bz' a' b' c' digit' (i + 1)
                else (if roundDown then b else b + 1, i)
