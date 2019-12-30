module Text.Format.Floating.Ryu where

import Text.Format.Floating.Decimal
import Text.Format.Floating.Ryu.Tables
import Text.Format.Floating.Rounding

import Data.Text (Text)

formatDec :: (RyuFloat a) => a -> Text
formatDec = formatDec' RoundEven

formatDec' :: (RyuFloat a) => RoundingMode -> a -> Text
formatDec' rm x = renderDecimal (rebaseDecimal x rm)

-- | This always uses a lowercase @e@ in the output. Compose with 'Data.Text.toUpper'
--   if uppercase output is desired.
formatSci :: (RyuFloat a) => a -> Text
formatSci = formatSci' RoundEven

formatSci' :: (RyuFloat a) => RoundingMode -> a -> Text
formatSci' rm x = renderDecimalSci (rebaseDecimal x rm)

class RealFloat a => RyuFloat a where
  -- | Note that implementations should not actually
  --   evaluate their argument; they are just used for the benefit of the type checker,
  --   in the same vein as Haskell's 'floatDigits' etc.
  ryuMultiplier :: a -> Integer

instance RyuFloat Float where
  ryuMultiplier x = floatTable (toInteger e2) where e2 = snd (decodeFloat x) - 2

instance RyuFloat Double where
  ryuMultiplier x = doubleTable (toInteger e2) where e2 = snd (decodeFloat x) - 2

rebaseDecimal :: RyuFloat a => a -> RoundingMode -> DecimalFloat
rebaseDecimal x = undefined
