{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Main where

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
populationAt4days = Map.fromList [(0,1), (1,0), (2,0), (3,0), (4,1), (5,1), (6,3), (7,1), (8,2)]
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
    it "should grow the population by a given number of days" $ do
      growPopulationByDays 4 population `shouldBe` populationAt4days 
    it "should dcrement the timer correctly" $ do
          decrementTimer (0, 1) `shouldBe` (-1,1)
          decrementTimer (1, 1) `shouldBe` (0,1)
          decrementTimer (1, 4) `shouldBe` (0,4) -- need more cases with quantity > 1
          decrementTimer (2, 1) `shouldBe` (1,1)
          decrementTimer (3, 1) `shouldBe` (2,1)
          decrementTimer (4, 1) `shouldBe` (3,1)
          decrementTimer (5, 1) `shouldBe` (4,1)
          decrementTimer (6, 1) `shouldBe` (5,1)
          decrementTimer (7, 1) `shouldBe` (6,1)
          decrementTimer (8, 1) `shouldBe` (7,1)
    it "should return the number of -1s aka newborn fish" $ do
          newBorn (Map.fromList [(-1,1)]) `shouldBe` 1
    it "should count the population" $ do
          countPopulation (Map.toList  populationAt4days) `shouldBe` 9
          countPopulation (Map.toList (growPopulationByDays 80 population)) `shouldBe` 5934
          countPopulation (Map.toList (growPopulationByDays 256 population)) `shouldBe` 26984457539


