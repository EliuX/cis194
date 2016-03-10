{-# OPTIONS_GHC -Wall #-}
module HomeWork5.CalcSpec (main, spec) where

import Test.Hspec
import HomeWork5.Calc
import HomeWork5.ExprT
import HomeWork5.Parser

main :: IO()
main = hspec spec 

spec :: Spec
spec = do
 describe "eval" $ do
  it "evals an ExprT expression calculating the contained math expression in it" $ do
   eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20
   eval (Mul (Lit 6) (Add (Lit 4) (Lit 2))) `shouldBe` 36
 describe "evalStr" $ do
  it "Produces a Just Integer for an expression or nothing to non well formed expressions" $ do
   evalStr "(2+3)*4" `shouldBe` Just 20
   evalStr "2+3*4" `shouldBe` Just 14
   evalStr "2+3*" `shouldBe` Nothing