{-# LANGUAGE TemplateHaskellQuotes #-}

module Text.Format.Floating.Ryu.Tables.TH (makeLookupTable) where

import Language.Haskell.TH

import Control.Applicative (liftA2)
import Data.Array (Array, array)

makeLookupTable :: String -> (Integer, Integer) -> [(Integer, Integer)] -> Q [Dec]
makeLookupTable n bounds mappings = liftA2 (:) fnSig fnDec where
  fnName        = mkName n
  fnSig         = sigD fnName [t| Array Integer Integer |]
  fnEqns        = [clause [] (normalB [| array bounds mappings |]) []]
  fnDec         = pure <$> funD fnName fnEqns
