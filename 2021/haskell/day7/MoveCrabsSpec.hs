{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Main where

import Test.Hspec
import MoveCrabs

testFileContents = "16,1,2,0,4,2,7,1,2,14\n"
positions = [0,1,1,2,2,2,4,7,14,16]
frequencyMap = [(0,1), (1,2), (2,3), (4,1), (7,1), (14,1), (16,1)]
costOfMovingZero = [(0,0), (1,2), (2,6), (4,4), (7,7), (14,14), (16,16)]
costOfMovingOne = [(0,1), (1,0), (2,3), (4,3), (7,6), (14,13), (16,15)]
uniquePostitions = [0..16]


main = hspec $ do
  describe "MoveCrabs tests" $ do
    it "should create a sorted list of positions from the test file loaded as a String" $ do
      sortedPositions testFileContents `shouldBe` positions
    it "should create a frequency  list" $ do
      frequency positions `shouldBe` frequencyMap
    it "should create a sorted list of unique postitions" $ do
      rawPositions frequencyMap `shouldBe` [0..16]
    it "should find the cost of a single move" $ do
      costOfMove 0 frequencyMap `shouldBe` costOfMovingZero
      costOfMove 1 frequencyMap `shouldBe` costOfMovingOne
    it "should find the total cost of a move" $ do
      totalCostOfMove costOfMovingZero `shouldBe` 49
      totalCostOfMove costOfMovingOne `shouldBe` 41
    it "should calculate the total of all the move costs " $ do
      calculateMoveCosts uniquePostitions frequencyMap `shouldBe` [(0,49),(1,41),(2,37),(3,39),(4,41),(5,45),(6,49),(7,53),(8,59),(9,65),(10,71),(11,77),(12,83),(13,89),(14,95),(15,103),(16,111)]
    it "should calculate the lowest cost move to align to the same postition" $ do
      cheapestMove frequencyMap `shouldBe` 37 
 
