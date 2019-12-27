{-# LANGUAGE GeneralisedNewtypeDeriving, ScopedTypeVariables #-}

module Text.Format.Floating.IEEE
  ( IEEEFloat(..)
  , IEEEComponents(..)
  , Sign
  , signValue
  , Exponent
  , exponentValue
  , Mantissa
  , mantissaValue
  , showBits
  )
where

import           Text.Format.Floating.IEEE.Constants

import           Data.Bits
import           GHC.Float                      ( castFloatToWord32
                                                , castDoubleToWord64
                                                )

class RealFloat a => IEEEFloat a where
    ieeeDecodeFloat :: a -> IEEEComponents a

    -- | These methods do not evaluate their argument.
    ieeeFloatBias :: a -> Integer
    ieeeExponentLength :: a -> Integer
    ieeeMantissaLength :: a -> Integer

instance IEEEFloat Float where
  ieeeDecodeFloat x = unsafeDecodeFloat (toInteger $ castFloatToWord32 x)
                                        float32ExponentLength
                                        float32MantissaLength
  ieeeFloatBias      = const float32Bias
  ieeeExponentLength = const float32ExponentLength
  ieeeMantissaLength = const float32MantissaLength

instance IEEEFloat Double where
  ieeeDecodeFloat x = unsafeDecodeFloat (toInteger $ castDoubleToWord64 x)
                                        float64ExponentLength
                                        float64MantissaLength
  ieeeFloatBias      = const float64Bias
  ieeeExponentLength = const float64ExponentLength
  ieeeMantissaLength = const float64MantissaLength

unsafeDecodeFloat :: Integer -> Integer -> Integer -> IEEEComponents a
unsafeDecodeFloat bits es ms = IEEEComponents (Sign s)
                                              (Exponent e)
                                              (Mantissa m) where
  ms' = fromInteger ms
  es' = fromInteger es
  m   = maskRange 0 ms' .&. bits
  e   = rotateR (maskRange ms' es' .&. bits) ms'
  s   = testBit bits (ms' + es')

-- | @mantissaMask l s@ is an 'Integer' mask for the @s@ consecutive bits starting
--   at index @l@. For example: @mantissaMask 4 6 == 0b0001111110000@.
maskRange :: Int -> Int -> Integer
maskRange _ 0 = zeroBits
maskRange l s = bit (l + s - 1) .|. maskRange l (s - 1)

data IEEEComponents a = IEEEComponents
    { ieeeSign      :: Sign a
    , ieeeExponent  :: Exponent a
    , ieeeMantissa  :: Mantissa a
    } deriving (Eq, Show)

-- | This type carries a 'Bool' representing the sign bit of an IEEE floating-point number.
--   @Sign False@ indicates a positive number; @Sign True@ indicates a negative.
newtype Sign a = Sign { signValue :: Bool } deriving (Eq, Bits, FiniteBits, Show)

-- | This type carries an arbitrary-precision integer representing the exponent of an IEEE
--   floating-point number, interpreted directly as an integer. For example:
--
--   (-2.0 :: Float) is encoded in 32 bits as @1 10000000 00000000000000000000000@
--   (most significant bit to the left). The exponent bits are the 8 bits in the middle; so
--   the corresponding 'Exponent' value would be @Exponent 128@.
--
--   The 32-bit float @0 10000010 00010011001100110011010@ (a smidge over 8.6) has a
--   'Mantissa' of @Mantissa 629146@, since @(0b00010011001100110011010 :: Int) = 629146@.
--
--   The type parameter is there to carry around the actual 'RealFloat' instance that this
--   number came from. Otherwise, since 'Integer' is an arbitrary-precision type, we would have
--   no way to determine the actual number of "significant" (sorry) bits in the value.
newtype Exponent a = Exponent { exponentValue :: Integer } deriving (Eq, Bits, Show)

instance IEEEFloat a => FiniteBits (Exponent a) where

    -- | This implementation does not evaluate its argument.
  finiteBitSize _ = fromInteger $ ieeeExponentLength (undefined :: a)

-- | This type carries an arbitrary-precision integer representing the mantissa of an IEEE
--   floating-point number, interpreted directly as an integer. For example:
--
--   (-2.0 :: Float) is encoded in 32 bits as @1 10000000 00000000000000000000000@
--   (most significant bit to the left). The mantissa bits are the 23 bits at the end; so
--   the corresponding 'Mantissa' value would be @Mantissa 0@.
--
--   The 32-bit float @0 10000010 00010011001100110011010@ (a smidge over 8.6) has a
--   'Mantissa' of @Mantissa 629146@, since @(0b00010011001100110011010 :: Int) = 629146@.
--
--   The type parameter is there to carry around the actual 'RealFloat' instance that this
--   number came from. Otherwise, since 'Integer' is an arbitrary-precision type, we would have
--   no way to determine the actual number of "significant" (sorry) bits in the value.
newtype Mantissa a = Mantissa { mantissaValue :: Integer } deriving (Eq, Bits, Show)

instance IEEEFloat a => FiniteBits (Mantissa a) where

    -- | This is the number of actual encoded bits in the mantissa of this float, /not/ including the
    --   implicit bit (which Haskell's floatDigits does include).
    --
    --   This implementation does not evaluate its argument.
  finiteBitSize _ = fromInteger $ ieeeMantissaLength (undefined :: a)

showBits :: (FiniteBits b) => b -> String
showBits b = reverse (buildBitsString 0) where
  buildBitsString i | i >= finiteBitSize b = []
                    | otherwise            = bitChar i : buildBitsString (i + 1)
  bitChar i = if testBit b i then '1' else '0'
