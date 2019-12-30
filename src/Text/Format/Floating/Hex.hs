module Text.Format.Floating.Hex where

import Data.Text (Text)
import qualified Data.Text as T

-- | This formats the float value in the style of C printf's @%a@ specifier. It always
--   uses lowercase letters; compose with 'Data.Text.toUpper' as needed. Note that this
--   format does not actually use the Ryu algorithm; it is included just for completeness.
formatHex :: RealFloat a => a -> Text
formatHex x = undefined
