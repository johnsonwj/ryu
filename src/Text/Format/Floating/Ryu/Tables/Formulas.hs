{-# LANGUAGE ScopedTypeVariables #-}

module Text.Format.Floating.Ryu.Tables.Formulas (multiplierTable) where

import Data.Bits (shift)

multiplierTable :: forall a. RealFloat a => a -> Integer -> Integer -> [(Integer, Integer)]
multiplierTable _ b0 b1 = map (\e2 -> (e2, multiplier e2 b0 b1)) e2s where
  (e2lo, e2hi) = e2Range (undefined :: a)
  e2s = [e2lo .. e2hi]

ryuQ :: Integer -> Integer
ryuQ e2
  | e2 < 0    = max 0 $ (-e2) *. logTenFive
  | otherwise = max 0 $ e2 *. logTenTwo

ryuK0, ryuK1 :: Integer -> Integer -> Integer
ryuK0 e2 b0 = b0 + ryuQ e2 *. logTwoFive
ryuK1 e2 b1 = (e2 + ryuQ e2) *. logTwoFive + b1

multiplier :: Integer -> Integer -> Integer -> Integer
multiplier e2 b0 b1
  -- 5^i / 2^k, where i == -e2 - q
  | e2 < 0    = (5 ^ (-e2 - ryuQ e2)) `shift` fromInteger (ryuK1 e2 b1)

  -- 2^k / 5^q + 1
  | otherwise = 2 ^ ryuK0 e2 b0 `div` 5 ^ ryuQ e2 + 1

-- | Gives the min and max values that @e2@ can take for a given float type.
--   This function does not evaluate its argument.
e2Range ::forall a.  RealFloat a => a -> (Integer, Integer)
e2Range _ = (e2min, e2max) where
  x = undefined :: a
  (emin, emax) = floatRange x
  sdigs = floatDigits x
  e2min = toInteger $ emin - sdigs - 2
  e2max = toInteger $ emax - sdigs - 2

logTenTwo, logTwoFive, logTenFive :: Float
logTenTwo   = logBase 10 2
logTwoFive  = logBase 2 5
logTenFive = logBase 10 5

infixl 7 *.
(*.) :: Integer -> Float -> Integer
s *. f = floor (fromInteger s * f)
