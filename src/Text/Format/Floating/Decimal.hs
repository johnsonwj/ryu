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
  deriving (Show)

renderDecimal :: DecimalFloat -> Text
renderDecimal = doRender normalRenderDec where
  normalRenderDec s e d
    | e == 0  = T.append (renderSignedDecimalInteger s d) ".0"
    | e >  0  = T.concat [renderSignedDecimalInteger s d, T.replicate (fromInteger e) "0", ".0"]
    | otherwise = let dr = renderSignedDecimalInteger False d
                      dl = T.length dr
                      dd = dl + fromInteger e
                  in if dd <= 0
                    then T.concat [signChar s, "0.", T.replicate (abs dd) "0", dr]
                    else let (w, f) = T.splitAt dd dr in T.concat [signChar s, w, ".", f]

  signChar True = "-"
  signChar False = ""

renderDecimalSci :: DecimalFloat -> Text
renderDecimalSci = doRender normalRenderSci where
  normalRenderSci s e d = T.concat [dh', ".", dt', "e", es, renderSignedDecimalInteger False (abs e')] where
    (dh, dt)  = T.splitAt 1 (renderSignedDecimalInteger False d)
    dh'       = if s then T.cons '-' dh else dh
    dt'       = if T.null dt then "0" else dt
    e'        = e + fromIntegral (T.length dt)
    es        = if e' < 0 then "-" else "" -- follow haskell convention where positive exponents do not have a +, as in 1.234e1

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
