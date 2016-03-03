{-# OPTIONS_GHC -Wall #-}
module HomeWork3.GolfSpec (main, spec) where

import Test.Hspec
import HomeWork3.Golf

main :: IO()
main = hspec spec

spec :: Spec
spec = do
 describe "skips" $ do
   it "A list of list of differents combinations of the passed list" $ do
    skips "ABCD"       `shouldBe` ["ABCD", "BD", "C", "D"]
    skips "hello!"     `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]
    skips [1]          `shouldBe` [[1]]
    skips [True,False] `shouldBe` [[True,False], [False]]
--    skips []           `shouldBe` []