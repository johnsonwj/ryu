module Text.Format.Floating.Ryu where

import Text.Format.Floating.Decimal
import Text.Format.Floating.Constants

import           Data.Text                      ( Text )

formatDec :: (RyuFloat a) => a -> Text
formatDec = renderDecimal . rebaseDecimal

-- | This always uses a lowercase @e@ in the output. Compose with 'Data.Text.toUpper'
--   if uppercase output is desired.
formatSci :: (RyuFloat a) => a -> Text
formatSci = renderDecimalSci . rebaseDecimal

class RealFloat a => RyuFloat a where
  -- | These represent the number of bits needed to store the multipliers used by
  --   the algorithm; see Section 3.2.4 in Adams' original paper for more information
  --   on how they were derived. Note that implementations should not actually
  --   evaluate their argument; they are just used for the benefit of the type checker,
  --   in the same vein as Haskell's 'floatDigits' etc.
  ryuB0 :: a -> Integer
  ryuB1 :: a -> Integer

instance RyuFloat Float where
  ryuB0 = const ryuFloatB0
  ryuB1 = const ryuFloatB1

instance RyuFloat Double where
  ryuB0 = const ryuDoubleB0
  ryuB1 = const ryuDoubleB1

rebaseDecimal :: RyuFloat a => a -> DecimalFloat
rebaseDecimal x = undefined
