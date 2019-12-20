{-# LANGUAGE GeneralisedNewtypeDeriving, ScopedTypeVariables #-}

module Text.Format.Floating.IEEE
    ( ieeeDecodeFloat
    , ieeeFloatBias
    , IEEEFloat, ieeeSign, ieeeExponent, ieeeMantissa
    , Sign, signValue, floatSign
    , Exponent, exponentValue, floatExponent
    , Mantissa, mantissaValue, floatMantissa
    , showBits
    ) where

import Data.Bits

{-|
    Haskell's 'decodeFloat' function is somewhat inconvenient for our purposes. Ryu defines its
    variables like this:
        normalizedFloat = (-1)^s * (1.m) * 2^(e - bias)
        denormalizedFloat = (-1)^s * (0.m) * 2^(1 - bias) [exponent bits are all 0]
        
-}

ieeeDecodeFloat :: (RealFloat a) => a -> Maybe (IEEEFloat a)
ieeeDecodeFloat x
    | isIEEE x  = Just (ieeeDecodeFloatUnsafe x)
    | otherwise = Nothing

ieeeDecodeFloatUnsafe :: (RealFloat a) => a -> IEEEFloat a
ieeeDecodeFloatUnsafe x = IEEEFloat (floatSign x) (floatExponent x) (floatMantissa x)

-- | This function does not evaluate its argument.
ieeeFloatBias :: forall a. (RealFloat a) => IEEEFloat a -> Int
ieeeFloatBias _ = let biasedExpMax = snd $ floatRange (undefined :: a) in biasedExpMax - 1

data IEEEFloat a = IEEEFloat
    { ieeeSign      :: Sign a
    , ieeeExponent  :: Exponent a
    , ieeeMantissa  :: Mantissa a 
    } deriving (Eq, Show)

-- | This type carries a 'Bool' representing the sign bit of an IEEE floating-point number.
--   @Sign False@ indicates a positive number; @Sign True@ indicates a negative.
newtype Sign a = Sign { signValue :: Bool } deriving (Eq, Bits, FiniteBits, Show)

floatSign :: (RealFloat a) => a -> Sign a
floatSign x = Sign (signum x < 0)

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

instance RealFloat a => FiniteBits (Exponent a) where

    -- | This implementation does not evaluate its argument.
    finiteBitSize _ = nbitsExp where
        (expMin, expMax) = floatRange (undefined :: a)

        -- +2 is because Haskell 'floatRange' does not include the min/max /encodeable/
        -- exponents (all zeroes or all ones, respectively): only the range of exponents
        -- for a normalized float value. For instance, a 32-bit float has 8 bits in the
        -- exponent, for a maximum unbiased exponent of @0b11111111 :: Int = 255@.
        -- Meanwhile, @floatRange (undefined :: Float) = (-125, 128)@ has a difference
        -- of 253, because it does not include the (biased) lowest value -126 or the
        -- (biased) highest value of 129.
        unbiasedMax = (expMax - expMin) + 2

        -- 'floatRange' returns a pair of 'Int's, which is a fixed-precision type
        -- (the actual size of the 'Int' type doesn't matter here). This enables
        -- a bit of a shortcut: @unbiasedMax@ will look like @0b0..01..1@
        -- where the number of ones is the same as the size of the exponent
        nbitsExp = finiteBitSize unbiasedMax - countLeadingZeros unbiasedMax

floatExponent :: forall a. RealFloat a => a -> Exponent a
floatExponent x = Exponent (toInteger unscaledExp) where
    unscaledExp = scaledBiasedExponent + bias - scale

    -- Haskell's 'exponent' returns the biased exponent, scaled so that 'significand' will
    -- return a number in the open interval (-1, 1). For denormalized numbers, this is
    -- equivalent to the biased exponent, because the significand is already less than one
    -- in absolute value. But for normalized numbers, the implicit leading bit means that
    -- the significand will be divided by two, increasing the exponent by one. So we have
    -- to undo that here.
    scaledBiasedExponent = exponent x
    bias = ieeeFloatBias (undefined :: IEEEFloat a)
    scale = if isDenormalized x then 0 else 1


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

instance RealFloat a => FiniteBits (Mantissa a) where

    -- | This is the number of actual encoded bits in the mantissa of this float, /not/ including the
    --   implicit bit (which Haskell's floatDigits does include).
    --
    --   This implementation does not evaluate its argument.
    finiteBitSize _ = floatDigits (undefined :: a) - 1

floatMantissa :: forall a. RealFloat a => a -> Mantissa a
floatMantissa x = Mantissa rescaledMantissa where
    (scaledSignificand, scaledExponent) = decodeFloat x

    mantissaDigits = finiteBitSize (undefined :: Mantissa a)

    rescaledSignificand = scaledSignificand * (floatRadix x ^ (scaledExponent + mantissaDigits - 1))

    rescaledMantissa = clearBit rescaledSignificand mantissaDigits

showBits :: (FiniteBits b) => b -> String
showBits b = reverse (buildBitsString 0) where
    buildBitsString i
        | i >= finiteBitSize b  = []
        | otherwise             = bitChar i : buildBitsString (i + 1)
    bitChar i = if testBit b i then '1' else '0'
