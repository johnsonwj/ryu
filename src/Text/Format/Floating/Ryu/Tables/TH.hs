{-# LANGUAGE TemplateHaskellQuotes #-}
module Text.Format.Floating.Ryu.Tables.TH (makeLookupTable) where

import           Language.Haskell.TH

import           Control.Applicative            ( liftA2 )

makeLookupTable :: String -> [(Integer, Integer)] -> Q [Dec]
makeLookupTable n ps = liftA2 (:) fnSig fnDec where
  fnName        = mkName n
  fnSig         = sigD fnName [t| Integer -> Integer |]
  entryClauses  = makeLookupEntry <$> ps
  fnEqns        = entryClauses ++ [clause [wildP] (normalB [| error "no entry defined" |]) []]
  fnDec         = pure <$> funD fnName fnEqns

makeLookupEntry :: (Integer, Integer) -> Q Clause
makeLookupEntry (x, y) = clause [litP (IntegerL x)] (normalB . litE $ IntegerL y) []
