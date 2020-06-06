{-# LANGUAGE OverloadedStrings #-}

module Text.Format.Floating.Decimal where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (intToDigit)

-- | 'True' is negative, 'False' is positive; just like the sign bit in an IEEE float.
type SignBit = Bool

data DecimalFloat
  = DecimalNaN Integer
  -- ^ The integer argument is the significand of the (binary) float, which might have some implementation-specific meaning
  | DecimalInfinity SignBit
  | DecimalFloat SignBit Integer Integer
  -- ^ DecimalFloat s e10 d10 represents the number @(-1)^s * d10 * 10^e10@ where @d10@ is the smallest possible of the range
  --   of real numbers that could be represented by the original (binary) encoding.

renderDecimal :: DecimalFloat -> Text
renderDecimal = doRender normalRenderDec where
  normalRenderDec s e d
    | e == 0  = T.append (renderSignedDecimalInteger s d) ".0"
    | e >  0  = T.concat [renderSignedDecimalInteger s d, T.replicate (fromInteger e) "0", ".0"]
    | otherwise = let dr = renderSignedDecimalInteger s d
                      dl = T.length dr
                      dd = dl + fromInteger e
                  in if dd <= 0
                    then T.concat ["0.", T.replicate (abs dd) "0", dr]
                    else let (w, f) = T.splitAt dd dr in T.concat [w, ".", f]

renderDecimalSci :: DecimalFloat -> Text
renderDecimalSci = doRender normalRenderSci where
  normalRenderSci s e d = T.concat [dh', ".", dt', "e", es, renderSignedDecimalInteger False (abs e')] where
    (dh, dt)  = T.splitAt 1 (renderSignedDecimalInteger False d)
    dh'       = if s then T.cons '-' dh else dh
    dt'       = if T.null dt then "0" else dt
    e'        = e + fromIntegral (T.length dt)
    es        = if e' < 0 then "-" else "+"

type NonsingularRenderer = SignBit -> Integer -> Integer -> Text

doRender :: NonsingularRenderer -> DecimalFloat -> Text
doRender _ (DecimalNaN _)           = "NaN"
doRender _ (DecimalInfinity True)   = "-Infinity"
doRender _ (DecimalInfinity False)  = "Infinity"
doRender r (DecimalFloat s e d)     = r s e d

renderSignedDecimalInteger :: SignBit -> Integer -> Text
renderSignedDecimalInteger False 0 = "0"
renderSignedDecimalInteger True  0 = "-0"
renderSignedDecimalInteger s x = addSign (renderAbs x) where
  addSign = if s then T.cons '-' else id
  
  renderAbs i =
    let (q, r)  = i `quotRem` 10
        rc      = intToDigit $ fromIntegral r
    in if q > 0
      then T.snoc (renderAbs q) rc
      else T.singleton rc

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