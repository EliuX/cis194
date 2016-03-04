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
 describe "localMaxima" $ do
   it "A list which is strictly greater than both the elements immediately before and after it" $ do
	localMaxima [2,9,5,6,1] `shouldBe` [9,6]
	localMaxima [2,3,4,1,5] `shouldBe` [4]
	localMaxima [1,2,3,4,5] `shouldBe` []
 describe "histogram" $ do
   it "Shows a vertical histogram showing how many of each number were in the input list" $ do
	histogram [3,5]   `shouldBe` "  * *    \n==========\n0123456789\n"
	histogram [1,8,1] `shouldBe` "*        \n*      * \n==========\n0123456789\n"

