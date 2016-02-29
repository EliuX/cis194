module HomeWork1.CreditCardValidationSpec (main, spec) where

import Test.Hspec
import HomeWork1.CreditCardValidation

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "toDigits" $ do
    it "converts an Integer to a List of Digits" $ do
      toDigits 1234 `shouldBe` [1,2,3,4]
      toDigits 4321 `shouldBe` [4,3,2,1]
