{-# OPTIONS_GHC -Wall #-}
module HomeWork6.FibonacciSpec(main, spec) where

import Test.Hspec
import HomeWork6.Fibonacci

main :: IO()
main = hspec spec

spec :: Spec
spec = do
 describe "fib" $ do
  it "Calculates the fibonacci value of some number n" $ do
   fib 1 `shouldBe` 1
   fib 2 `shouldBe` 1
   fib 3 `shouldBe` 2
   fib 4 `shouldBe` 3
   fib 5 `shouldBe` 5
   fib 11 `shouldBe` 89
 describe "fibl" $ do
  it "Calculates the infinite fibonacci numbers" $ do
   let firstNumbers = [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]
   take (length firstNumbers)  fibs1 `shouldBe` firstNumbers
