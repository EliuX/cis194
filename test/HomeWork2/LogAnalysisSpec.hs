{-# OPTIONS_GHC -Wall #-}
module HomeWork2.LogAnalysisSpec (main, spec) where

import Test.Hspec
import System.IO  
import Control.Monad
import HomeWork2.LogAnalysis
import HomeWork2.Log

main :: IO()
main = hspec spec


spec :: Spec
spec = do
 describe "parseMessage" $ do
   it "parses an individual log message" $ do
      parseMessage "E 2 562 help help" `shouldBe` LogMessage (Error 2) 562 "help help"
      parseMessage "I 29 la la la" `shouldBe` LogMessage Info 29 "la la la"
      parseMessage "This is not in the right format" `shouldBe` Unknown "This is not in the right format"
 describe "parse" $ do 
   it "parses a whole log file" $ do 
      testParse parse 10 "src/HomeWork2/error.log"  
      True `shouldBe` not False 