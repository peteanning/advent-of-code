{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module HydrothermalVentureSpec where

import Test.Hspec
import HydrothermalVenture

line1 = ((0,0), (9,9)) :: Line
line2 = ((9,9), (0,0))
line3 = ((0,5), (0,1))


main :: IO ()
main = hspec $ do
  describe "HydrothermalVenture tests" $ do
    it "should make a Tuple from a String array [1,2] as (1,2)" $ do
      mkTuple  ["1","2"]  `shouldBe` (1,2)   
    it "should parse a String to a Tuple \"1,2\" to (1,2) " $ do
      parseTuple "1,2" `shouldBe` (1,2)   
    it "should parse a [String] of [\"0,9\", \"->\", \"1,2\"] to ((0,9), (1,2))" $ do
      parseWords ["0,0", "->", "9,9"] `shouldBe`  line1
    it "should convert a Line to an array [Pos]" $ do
      linesCover ((0,0), (0,3)) `shouldBe` [(0,0), (0,1), (0,2), (0,3)] 
      linesCover ((0,0), (3,0)) `shouldBe` [(0,0), (1,0), (2,0), (3,0)]
    it "should count the number of covered points" $ do
      pointCount [(0,0), (0,1), (0,2), (0,3), (0,0), (0,1)]  `shouldBe` 2  
      pointCount [(0,0), (0,1), (0,1), (0,3), (0,0), (0,1)]  `shouldBe` 2  
    it "should order the positons in a line" $ do
      orderLine line1 `shouldBe` line1 
      orderLine line2 `shouldBe` line1
      orderLine line3 `shouldBe` ((0,1), (0,5))



