{-# OPTIONS_GHC -Wall #-}
module HomeWork1.HanoiTowerSpec (main, spec) where

import Test.Hspec
import HomeWork1.HanoiTower

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "hanoi" $ do
    it "return a list of moves to be performed to move the stack of discs from the first peg to the seconds" $ do
      hanoi 2 "a" "b" "c" `shouldBe` [("a","c"), ("a","b"), ("c","b")]
      hanoi 3 "c" "a" "b" `shouldBe` [("c","a"), ("c","b"), ("a","b"), ("c","a"), ("b","c"), ("b","a"), ("c","a")]
