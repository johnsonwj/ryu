{-|
    This implements the 'basic conversion routine' given in Section 2 of Adams' original paper,
    without using lookup tables to speed up the loops. 
-}

{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Text.Format.Floating.Simple where -- (format, formatDec, formatSci, formatHex) where

import Text.Format.Floating.IEEE

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (intToDigit)
import Data.Bits (popCount, finiteBitSize)

formatDec :: (IEEEFloat a) => a -> Text
formatDec = formatDecimal renderDec

formatSci :: (IEEEFloat a) => a -> Text
formatSci = formatDecimal renderSci

formatHex :: (IEEEFloat a) => a -> Text
formatHex x = undefined where
    IEEEComponents cs ce cm = ieeeDecodeFloat x
    m = mantissaValue cm
    e = exponentValue ce

type Renderer = Bool -> Integer -> Integer -> Text

formatDecimal :: (IEEEFloat a) => Renderer -> a -> Text
formatDecimal render x = case convertToDecimal x of
    NaN     -> "NaN"
    PosInf  -> "Infinity"
    NegInf  -> "-Infinity"
    DecimalFloat s d e  -> render s d e

renderDec :: Renderer
renderDec s d e
    | e == 0    = T.append (renderIntegerDecimal s d) ".0"
    | e > 0     = T.concat [renderIntegerDecimal s d, T.replicate (fromInteger e) "0", ".0"]
    | otherwise = let
        dr = renderIntegerDecimal s d
        dl = T.length dr
        dd = dl + fromInteger e
        in if dd <= 0
            then T.concat ["0.", T.replicate (abs dd) "0", dr]
            else let (w, f) = T.splitAt dd dr in T.concat [w, ".", f]

renderSci :: Renderer
renderSci s d e = T.concat [dh', ".", dt', "e", es, renderIntegerDecimal False $ abs e'] where
    (dh, dt) = T.splitAt 1 (renderIntegerDecimal False d)
    dh' = if s then T.cons '-' dh else dh
    dt' = if T.null dt then "0" else dt
    e' =  e + fromIntegral (T.length dt)
    es = if e' < 0 then "-" else "+"

data DecimalFloat = NaN | PosInf | NegInf | DecimalFloat Bool Integer Integer deriving (Eq, Show)

-- | Convert a given float to the form (m10, e10) where m10 is the smallest valid integer,
--   such that a = m10 * 10 ^ e10.
convertToDecimal :: (IEEEFloat a) => a -> DecimalFloat
convertToDecimal x
    | popCount ce == finiteBitSize ce   = case (s, m) of
        (False, 0)  -> PosInf
        (True, 0)   -> NegInf
        (_, _)      -> NaN
    | otherwise = let
        mf = if e == 0 then m else m + 2 ^ ml
        ef = (if e == 0 then 1 else e) - bias - ml

        e2 = ef - 2

        uvw = addIntervals (intervalOf $ 4 * mf) (FloatInterval (if m == 0 && e > 1 then -1 else -2, 2))

        (e10, FloatInterval (a, c)) = if e2 >= 0
            then (0, scaleInterval uvw (2 ^ e2))
            else (e2, scaleInterval uvw (5 ^ abs e2))

        
        (d0, e0) = shortestDecimalRep a c True True
        in DecimalFloat s d0 (e0 + e10)
    where
        IEEEComponents cs ce cm = ieeeDecodeFloat x

        s = signValue cs
        m = mantissaValue cm
        e = exponentValue ce

        ml = ieeeMantissaLength x
        bias = ieeeFloatBias x

newtype FloatInterval = FloatInterval (Integer, Integer)

scaleInterval :: FloatInterval -> Integer -> FloatInterval
scaleInterval (FloatInterval (a, c)) s = FloatInterval (a * s, c * s)

addIntervals :: FloatInterval -> FloatInterval -> FloatInterval
addIntervals (FloatInterval (a, c)) (FloatInterval (a', c')) = FloatInterval (a + a', c + c')

intervalOf :: Integer -> FloatInterval
intervalOf x = FloatInterval (x, x)

shortestDecimalRep :: Integer -> Integer -> Bool -> Bool -> (Integer, Integer)
shortestDecimalRep a0 c0 acceptSmaller acceptLarger = go True a0 (if acceptLarger then c0 else c0 - 1) 0 where
    go az a c i = let
        a' = a `div` 10
        c' = c `div` 10
        adz = a `mod` 10 == 0
        az' = az && adz
        in if  a' < c' || (acceptSmaller && az')
            then go az' a' c' (i + 1)
            else (c, i)


renderIntegerDecimal :: Bool -> Integer -> Text
renderIntegerDecimal False 0 = "0"
renderIntegerDecimal True 0 = "-0"
renderIntegerDecimal sgn x = let
    addSign = if sgn then T.cons '-' else id
    (q, r) = x `quotRem` 10
    rc = intToDigit $ fromIntegral r
    in addSign $ if q > 0
        then T.snoc (renderIntegerDecimal False q) rc
        else T.singleton rc
