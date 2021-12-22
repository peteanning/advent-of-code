module BingoSpece where

import Test.Hspec

import Bingo
import Data.List (transpose, permutations)

testFile :: String
testFile = "7,4,9,5,11,17,23\n\n22 13 17 11  0\n 8  2 23  4 24\n21  9 14 16  7\n 6 10  3 18  5\n 1 12 20 15 19\n\n" 
testgame = "7,4,9,5,11,17,23"
testgameStrArray = ["7","4","9","5","11","17","23"]
testgameArray = [(22,1), (13,2), (17,3), (11,4), (0,5)]
cards = ["", "22 13 17 11  0"," 8  2 23  4 24","21  9 14 16  7"," 6 10  3 18  5"," 1 12 20 15 19",""]
testgameInts = [(7,1),(4,2),(9,3),(5,4),(11,5),(17,6),(23,7)] :: [(Int,Int)]
parsedCards :: [[[Int]]]
parsedCards =[ [ [22, 13, 17, 11,  0],[ 8,  2, 23,  4, 24],[21,  9, 14, 16,  7],[ 6, 10,  3, 18,  5],[ 1, 12, 20, 15, 19] ]]
parsedCard = [ [22, 13, 17, 11,  0],[ 8,  2, 23,  4, 24],[21,  9, 14, 16,  7],[ 6, 10,  3, 18,  5],[ 1, 12, 20, 15, 19] ]
scoredCards :: [[[(Int,Int)]]]
scoredCards = [ [ [(22,1), (13,2), (17,3), (11,4),  (0,5)],[ (8,0),  (2,0), (23,0),  (4,0), (24,0)],[(21,0),  (9,0), (14,0), (16,0),  (7,0)],[ (6,0), (10,0),  (3,0), (18,0),  (5,0)],[ (1,0), (12,0), (20,0), (15,0), (19,0)] ]]
scoredCard = [ [(22,1), (13,2), (17,3), (11,4),  (0,5)],[ (8,0),  (2,0), (23,0),  (4,0), (24,0)],[(21,0),  (9,0), (14,0), (16,0),  (7,0)],[ (6,0), (10,0),  (3,0), (18,0),  (5,0)],[ (1,0), (12,0), (20,0), (15,0), (19,0)] ]
scoredCardNoLines = [ [(22,1), (13,2), (17,0), (11,4),  (0,5)],[ (8,0),  (2,0), (23,0),  (4,0), (24,0)],[(21,0),  (9,0), (14,0), (16,0),  (7,0)],[ (6,0), (10,0),  (3,0), (18,0),  (5,0)],[ (1,0), (12,0), (20,0), (15,0), (19,0)] ]
scoredCardColumnWin = [ [(22,1), (13,0), (17,0), (11,0),  (0,0)],[ (8,2),  (2,0), (23,0),  (4,0), (24,0)],[(21,3),  (9,0), (14,0), (16,0),  (7,0)],[ (6,4), (10,0),  (3,0), (18,0),  (5,0)],[ (1,5), (12,0), (20,0), (15,0), (19,0)] ]


main :: IO ()
main = hspec $ do
  describe "Bingo Tests" $ do
    it "fail to load a file that does not exist" $ do
      loadsetup "non-existent.txt" `shouldThrow` anyException   
    it "load a setup file" $ do
      loadsetup "test.txt" `shouldReturn` testFile
    it "should split a coma delimited string into an Int array" $ do
      split ',' testgame `shouldBe` testgameStrArray
    it "should split a coma delimited string into an Int array" $ do
     game testgame `shouldBe` testgameInts
    it "should trim strings so ' 0' becomes '0'" $ do
      trim " 0" `shouldBe` "0"
      trim "0 " `shouldBe` "0"
      trim " 0 " `shouldBe` "0"
    it "should parse cards" $ do
      card cards `shouldBe` parsedCards
    it "Play a Game and get a scored result" $ do
      play testFile `shouldBe` 0
    it "convert a number to a scored number when it is found in the list" $ do
      checkNumber 17 (zip [1..20] [1..]) `shouldBe` (17,17) 
      checkNumber 21 (zip [1..20] [1..]) `shouldBe` (21, 0) 
      checkNumber 28 [(28,1), (10,2)] `shouldBe` (28, 1) 
    it "scores a game by matching the marked number and its called order" $ do
      score parsedCard testgameArray `shouldBe` scoredCard      
    it "scores multiple game cards" $ do
      scoreGame parsedCards testgameArray `shouldBe` scoredCards
    it "should test a card to see if it is a Winning candidate" $ do
      isComplete scoredCard `shouldBe` True
      isComplete scoredCardNoLines `shouldBe` False
    it "should find a complete line row or col" $ do
      isCompleteLine [(1,1), (3,3), (99,2)] `shouldBe` True
      isCompleteLine [(1,0), (3,3), (99,2)] `shouldBe` False 
    it "should find the winning card with a completed row or column" $ do
      findWinner scoredCards `shouldBe` scoredCard
      findWinner [scoredCardNoLines] `shouldBe` []
      findWinner [scoredCardColumnWin] `shouldBe` transpose scoredCardColumnWin
      findWinner [[[(1,1)]]] `shouldBe` [[(1,1)]]
      findWinner [ [[(1,1)]], [[(2,0)]], [[(3,0)]] ] `shouldBe` [[(1,1)]]
      -- lets hope that is enough permutations  
      




