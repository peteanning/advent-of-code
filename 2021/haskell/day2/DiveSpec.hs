module DiveSpec where

import Test.Hspec

import Dive

main :: IO ()
main = hspec $ do
  describe "Dive tests" $ do
    
    it "Getting the result from a Pos of (10,10) shouldBe 100" $ do
      getResult (10,10) `shouldBe` 100

    it "Getting the result from a Pos of (0,0) shouldBe 0" $ do
      getResult (0,0) `shouldBe` 0


  -- starts from (0,0)
    it "Dive should return a position of (0,0) for the empty list and a current pos (0,0)" $ do
      getPos [] (0,0) `shouldBe` (0,0)
    
    it "Dive should return a position of (1,1) for the empty list and current pos (1,1)" $ do
      getPos [] (1,1) `shouldBe` (1,1)
 
    it "Dive should return  a position of (0,1) for a list for [down,1] current pos (0,0)" $ do
      getPos [("down",1)] (0,0) `shouldBe` (0,1)

    it "Dive should return  a position of (0,1) for a list for [forward,1] current pos (0,0)" $ do
      getPos [("forward",1)] (0,0) `shouldBe` (1,0)
  
    it "Dive should return  a position of (0,-1) for a list for [up,1] current pos (0,0)" $ do
      getPos [("up",1)] (0,0) `shouldBe` (0,-1)
  
    it "Dive should return  a position of (-99,-99) for a list for [unknown,1] current pos (0,0)" $ do
      getPos [("unknown",1)] (0,0) `shouldBe` (-99,-99)
  
  -- relative moves 
    it "Dive should return  a position of (0,2) for a list for [down,1] for a current pos(0,1)" $ do
      getPos [("down", 1)] (0,1) `shouldBe` (0,2)

    it "Dive should return  a position of (2,0) for a list for [forward,1] for a current pos(1,0)" $ do
      getPos [("forward", 1)] (1,0) `shouldBe` (2,0)

  -- multiple commandsRun

    it "Dive should return  a position of (1,1) for a list for [ forward,1, down,1] for a current pos(0,0)" $ do
      getPos [("forward", 1), ("down", 1) ] (0,0) `shouldBe` (1,1)




