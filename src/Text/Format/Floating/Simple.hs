{-|
    This implements the 'basic conversion routine' given in Section 2 of Adams' original paper,
    without using lookup tables to speed up the loops. 
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Text.Format.Floating.Simple (formatDec, formatSci) where

import Data.Text (Text)
import Data.Binary (Binary, Word8, encode)
import Data.ByteString.Lazy (unpack)
import Data.Bits

formatDec :: (RealFloat a) => a -> Text
formatDec = undefined

formatSci :: (RealFloat a) => a -> Text
formatSci = undefined

-- | Convert a given float to the form (m10, e10) where m10 is the smallest valid integer,
--   such that a = m10 * 10 ^ e10.
convertToDecimal :: (RealFloat a) => a -> (Integer, Int)
convertToDecimal a
    | isInfinite a                  = undefined --  if signum a < 0 then ('-' :)
    | floatRadix a == 10            = decodeFloat a
    | floatRadix a == 2 && isIEEE a = uncurry ieeeBinToDecimal (decodeFloat a)
    | otherwise                     = error "we only support IEEE floats with radix 2"

ieeeBinToDecimal :: Integer -> Int -> (Integer, Int)
ieeeBinToDecimal m2 e2 = undefined
