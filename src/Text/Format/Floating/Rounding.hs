module Text.Format.Floating.Rounding where

{- | For a given float value, this algorithm will output a decimal representation
     anywhere in the interval with endpoints halfway between the next-smallest and
     next-largest representable value. These endpoints may or may not themselves
     allowed representations, which is to say that they might have rounded to this
     encoded value---since after all the endpoints' actual values are by construction
     not representable with this machine float's precision.

     @RoundEven@ means that a number exactly between two encoded float values would
     round to the one with an even significand. So if this encoded value's significand
     is even, we know that the two midpoints would round here and are therefore
     valid representations.

     @RoundBigger@ and @RoundSmaller@ mean that a number exactly between two encoded
     float values would round to the one higher or lower /in absolute value/
     respectively. Thus the lower endpoint is valid (since it would round up), but
     not the upper endpoint (since it would round to the next-higher machine
     float, not this one).

     @RoundUp@ means that a number exactly between two encoded float values would round
     to the one closer to +Infinity; so only the lower bound is valid for positive numbers,
     and likewise only the upper bound is valid for negative numbers.

     @Exclusive@ means that the endpoints are never valid for a decimal representation;
     this is useful for instance if we don't know what rounding mode was used to
     encode this number as a machine float. To ensure that our decimal value would
     always encode to this float regardless of rounding, we do not allow either of
     the rounding-sensitive values.
 -}
data RoundingMode = RoundEven | RoundUp | RoundDown | RoundBigger | RoundSmaller | Exclusive

acceptLowerBound :: RoundingMode -> Integer -> Bool -> Bool
acceptLowerBound RoundEven    m _   = even m
acceptLowerBound RoundBigger  _ _   = True
acceptLowerBound RoundSmaller _ _   = False
acceptLowerBound RoundUp      _ neg = not neg
acceptLowerBound RoundDown    _ neg = neg
acceptLowerBound Exclusive    _ _   = False

acceptUpperBound :: RoundingMode -> Integer -> Bool -> Bool
acceptUpperBound RoundEven    m _   = even m
acceptUpperBound RoundBigger  _ _   = False
acceptUpperBound RoundSmaller _ _   = True
acceptUpperBound RoundUp      _ neg = neg
acceptUpperBound RoundDown    _ neg = not neg
acceptUpperBound Exclusive    _ _   = False

type FloatInterval = (Integer, Integer, Integer)

scaleInterval :: FloatInterval -> Integer -> FloatInterval
scaleInterval (a, b, c) s = (a * s, b * s, c * s)

addIntervals :: FloatInterval -> FloatInterval -> FloatInterval
addIntervals (a, b, c) (a', b', c') = (a + a', b + b', c + c')

intervalOf :: Integer -> FloatInterval
intervalOf x = (x, x, x)
