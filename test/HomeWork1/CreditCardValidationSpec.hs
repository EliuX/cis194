{-# OPTIONS_GHC -Wall #-}
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
      toDigits 0 `shouldBe` []
      toDigits (-17) `shouldBe` []
  describe "doubleEveryOther" $ do
    it "should double every other number beginning from the right" $ do
      doubleEveryOther [8,7,6,5] `shouldBe` [16,7,12,5]
      doubleEveryOther [1,2,3] `shouldBe` [1,4,3]
  describe "sumDigits" $ do
    it "to calculate the sum of all digits" $ do
      sumDigits [] `shouldBe` 0
      sumDigits [16,7,12,5] `shouldBe` 22
  describe "validate" $ do
    it "Validates if a credit card is valid" $ do
      validate 4012888888881881 `shouldBe` True
      validate 4012888888881882 `shouldBe` False
