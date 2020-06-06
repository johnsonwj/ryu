module Text.Format.Floating.SimpleSpec where

import Test.Hspec
import Text.Format.Floating.Simple

-- this borrows test values lifted directly from ulfjack's FloatToStringTest.java
-- todo: automatically generate test cases with QuickCheck?
spec :: Spec
spec = describe "Simple" $ do
    describe "formatDec" $ do
        it "correctly formats a float" $ do
            formatDec (4103.9003 :: Float) `shouldBe` "4103.9004"
