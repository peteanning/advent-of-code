module HydrothermalVentureSpec where

import Test.Hspec
import HydrothermalVenture

main :: IO ()
main = hspec $ do
  describe "HydrothermalVenture tests" $ do
    it "should make a Tuple from a String array [1,2] as (1,2)" $ do
      mkTuple  ["1","2"]  `shouldBe` (1,2)   
    it "should parse a String to a Tuple \"1,2\" to (1,2) " $ do
      parseTuple "1,2" `shouldBe` (1,2)   
    it "should parse a [String] of [\"0,9\", \"->\", \"1,2\"] to ((0,9), (1,2))" $ do
      parseWords ["0,9", "->", "1,2"] `shouldBe` ((0,9), (1,2))   



