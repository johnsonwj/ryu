module Text.Format.Floating.RyuSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Text.Format.Floating.Simple as Simple
import qualified Text.Format.Floating.Ryu as Ryu
import Text.Printf
import Data.Text (pack)

spec :: Spec
spec = describe "Ryu" $ do
    describe "formatDec" $ do
        describe "Float" $ do
            it "matches the output without lookup table" $ property prop_floatDecMatchesSimple
            it "matches the output of show" $ forAll floatsShowDec prop_floatDecMatchesShow
            it "matches the output of printf" $ property prop_floatDecMatchesPrintf
        describe "Double" $ do
            it "matches the output without lookup table" $ property prop_doubleDecMatchesSimple
            it "matches the output of show" $ forAll doublesShowDec prop_doubleDecMatchesShow
            it "matches the output of printf" $ property prop_doubleDecMatchesPrintf
    describe "formatSci" $ do
        describe "Float" $ do
            it "matches the output without lookup table" $ property prop_floatSciMatchesSimple
            it "matches the output of show" $ forAll floatsShowSci prop_floatSciMatchesShow
            it "matches the output of printf" $ property prop_floatSciMatchesPrintf
        describe "Double" $ do
            it "matches the output without lookup table" $ property prop_doubleSciMatchesSimple
            it "matches the output of show" $ forAll doublesShowSci prop_doubleSciMatchesShow
            it "matches the output of printf" $ property prop_doubleSciMatchesPrintf

-- Since `show` has its own rules as to when to print in scientific form, these generators
-- filter for only those floats/doubles which do/do not `show` with an exponent

floatsShowDec :: Gen Float
floatsShowDec = (arbitrary :: Gen Float) `suchThat` (\x -> 'e' `notElem` (show x))

floatsShowSci :: Gen Float
floatsShowSci = (arbitrary :: Gen Float) `suchThat` (\x -> 'e' `elem` (show x))

doublesShowDec :: Gen Double
doublesShowDec = (arbitrary :: Gen Double) `suchThat` (\x -> 'e' `notElem` (show x))

doublesShowSci :: Gen Double
doublesShowSci = (arbitrary :: Gen Double) `suchThat` (\x -> 'e' `elem` (show x))

prop_floatDecMatchesSimple :: Float -> Bool
prop_floatDecMatchesSimple x = Ryu.formatDec x == Simple.formatDec x

prop_floatDecMatchesShow :: Float -> Bool
prop_floatDecMatchesShow x = Ryu.formatDec x == pack (show x)

prop_floatDecMatchesPrintf :: Float -> Bool
prop_floatDecMatchesPrintf x = Ryu.formatDec x == pack (printf "%f" x)

prop_doubleDecMatchesSimple :: Double -> Bool
prop_doubleDecMatchesSimple x = Ryu.formatDec x == Simple.formatDec x

prop_doubleDecMatchesShow :: Double -> Bool
prop_doubleDecMatchesShow x = Ryu.formatDec x == pack (show x)

prop_doubleDecMatchesPrintf :: Double -> Bool
prop_doubleDecMatchesPrintf x = Ryu.formatDec x == pack (printf "%f" x)

prop_floatSciMatchesSimple :: Float -> Bool
prop_floatSciMatchesSimple x = Ryu.formatSci x == Simple.formatSci x

prop_floatSciMatchesShow :: Float -> Bool
prop_floatSciMatchesShow x = Ryu.formatSci x == pack (show x)

prop_floatSciMatchesPrintf :: Float -> Bool
prop_floatSciMatchesPrintf x = Ryu.formatSci x == pack (printf "%e" x)

prop_doubleSciMatchesSimple :: Double -> Bool
prop_doubleSciMatchesSimple x = Ryu.formatSci x == Simple.formatSci x

prop_doubleSciMatchesShow :: Double -> Bool
prop_doubleSciMatchesShow x = Ryu.formatSci x == pack (show x)

prop_doubleSciMatchesPrintf :: Double -> Bool
prop_doubleSciMatchesPrintf x = Ryu.formatSci x == pack (printf "%e" x)
