{-# LANGUAGE TemplateHaskellQuotes #-}
module Text.Format.Floating.Ryu.Tables where

import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))

lookupTable :: QuasiQuoter
lookupTable = QuasiQuoter
    { quoteExp  = decOnly
    , quotePat  = decOnly
    , quoteType = decOnly
    , quoteDec  = generateLookupTableDec
    }
    where
        decOnly = fail "lookup table quoter is only valid as a top-level declaration"

generateLookupTableDec :: String -> Q [Dec]
generateLookupTableDec = undefined


