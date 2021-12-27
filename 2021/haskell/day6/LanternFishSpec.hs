{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module LanternFishSpec where

import Test.Hspec
import LanternFish
import qualified Data.Map as Map

rawFish = "3,4,3,1,2"
rawFishDay1 = "2,3,2,0,1"

processedFish = [[1],[2], [3,3],[4]] :: [[Int]]
processedFishDay2 = [[0,0], [1,1], [2,2], [3]] :: [[Int]]

day1Fish = [[0],[1], [2,2],[3]] :: [[Int]]
day2Fish = [[0],[1], [2,2],[3]] :: [[Int]]
population = Map.fromList [(0,0),(1,1),(2,1), (3,2),(4,1),(5,0),(6,0),(7,0),(8,0)]
populationAt1days = Map.fromList [(0,1), (1,1), (2,2), (3,1), (4,0), (5,0), (6,0), (7,0), (8,0)]
extinct = Map.fromList [(0,0),(1,0),(2,0), (3,0)]

main = hspec $ do
  describe "Lantern Fish growth tests" $ do
    it "should load an array of fish grouped by their internal timers" $ do
      fishFromStrArray rawFish `shouldBe` processedFish
    it "should balance a Map of fish with missing members" $ do
       balanceFish 0 3 Map.empty `shouldBe` extinct 
    it "should load the fish as a Map of Fish and their quantity" $ do
      fishPopulation processedFish `shouldBe` population
    it "should grow the population" $ do  
      growPopulation population  `shouldBe` populationAt1days 
      
 
