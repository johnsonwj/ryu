module Text.Format.Floating.Ryu where

import Data.Text (Text)

formatDec :: (RealFloat a) => a -> Text
formatDec = undefined

-- | This always uses a lowercase @e@ in the output. Compose with 'Data.Text.toUpper'
--   if uppercase output is desired.
formatSci :: (RealFloat a) => a -> Text
formatSci = undefined

-- | This formats the float value in the style of C printf's @%a@ specifier. It always
--   uses lowercase letters; compose with 'Data.Text.toUpper' as needed.
formatHex :: (RealFloat a) => a -> Text
formatHex = undefined