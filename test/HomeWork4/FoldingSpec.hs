{-# OPTIONS_GHC -Wall #-}
module HomeWork4.FoldingSpec (main, spec) where

import Test.Hspec
import HomeWork4.Folding

main :: IO()
main = hspec spec

spec :: Spec
spec = do
 describe "fun1" $ do
    it "Multiplies every even number decreased in two" $ do 
     fun1 [10..20] `shouldBe` fun1' [10..20]
     fun1 [0,1,3] `shouldBe` fun1' [0,1,3]
 describe "fun2" $ do
    it "Given a number 'n' if its even it plus it and apply the function to its half, else it aplies the function to its triples plus 1" $ do
     fun2 3 `shouldBe` fun2' 3 
     fun2 7 `shouldBe` fun2' 7 