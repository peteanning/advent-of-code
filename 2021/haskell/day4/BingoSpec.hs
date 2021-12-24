module BingoSpec where

import Test.Hspec

import Bingo
import Trim
import Data.List (transpose, permutations)

testFile :: String
testFile = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\n22 13 17 11  0\n 8  2 23  4 24\n21  9 14 16  7\n 6 10  3 18  5\n 1 12 20 15 19\n\n 3 15  0  2 22\n 9 18 13 17  5\n19  8  7 25 23\n20 11 10 24  4\n14 21 16 12  6\n\n14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n 2  0 12  3  7\n"
testDraw = "7,4,9,5,11,17,23"
testDrawStrArray = ["7","4","9","5","11","17","23"]
testDrawArray = [(22,1), (13,2), (17,3), (11,4), (0,5)]
cards = ["", "22 13 17 11  0"," 8  2 23  4 24","21  9 14 16  7"," 6 10  3 18  5"," 1 12 20 15 19",""]
testDrawInts = [(7,1),(4,2),(9,3),(5,4),(11,5),(17,6),(23,7)] :: [(Int,Int)]
parsedBoards :: [[[Int]]]
parsedBoards =[ [ [22, 13, 17, 11,  0],[ 8,  2, 23,  4, 24],[21,  9, 14, 16,  7],[ 6, 10,  3, 18,  5],[ 1, 12, 20, 15, 19] ]]
parsedBoard = [ [22, 13, 17, 11,  0],[ 8,  2, 23,  4, 24],[21,  9, 14, 16,  7],[ 6, 10,  3, 18,  5],[ 1, 12, 20, 15, 19] ]
scoredBoards :: [[[(Int,Int)]]]
scoredBoards = [ [ [(22,1), (13,2), (17,3), (11,4),  (0,5)],[ (8,0),  (2,0), (23,0),  (4,0), (24,0)],[(21,0),  (9,0), (14,0), (16,0),  (7,0)],[ (6,0), (10,0),  (3,0), (18,0),  (5,0)],[ (1,0), (12,0), (20,0), (15,0), (19,0)] ]]
scoredBoard = [ [(22,1), (13,2), (17,3), (11,4),  (0,5)],[ (8,0),  (2,0), (23,0),  (4,0), (24,0)],[(21,0),  (9,0), (14,0), (16,0),  (7,0)],[ (6,0), (10,0),  (3,0), (18,0),  (5,0)],[ (1,0), (12,0), (20,0), (15,0), (19,0)] ]
scoredBoardNoLines = [ [(22,1), (13,2), (17,0), (11,4),  (0,5)],[ (8,0),  (2,0), (23,0),  (4,0), (24,0)],[(21,0),  (9,0), (14,0), (16,0),  (7,0)],[ (6,0), (10,0),  (3,0), (18,0),  (5,0)],[ (1,0), (12,0), (20,0), (15,0), (19,0)] ]
scoredBoardColumnWin = [ [(22,1), (13,0), (17,0), (11,0),  (0,0)],[ (8,2),  (2,0), (23,0),  (4,0), (24,0)],[(21,3),  (9,0), (14,0), (16,0),  (7,0)],[ (6,4), (10,0),  (3,0), (18,0),  (5,0)],[ (1,5), (12,0), (20,0), (15,0), (19,0)] ]


main :: IO ()
main = hspec $ do
  describe "Bingo Tests" $ do
    it "fail to load a file that does not exist" $ do
      playGameFromFile "non-existent.txt" `shouldThrow` anyException   
    it "load a setup file" $ do
      playGameFromFile "test.txt" `shouldReturn` testFile
    it "should split a coma delimited string into an Int array" $ do
      split ',' testDraw `shouldBe` testDrawStrArray
    it "should split a coma delimited string into an Int array" $ do
      parseDraw testDraw `shouldBe` testDrawInts
    it "should trim strings so ' 0' becomes '0'" $ do
      trim " 0" `shouldBe` "0"
      trim "0 " `shouldBe` "0"
      trim " 0 " `shouldBe` "0"
    it "should parse cards" $ do
      parseBoards cards `shouldBe` parsedBoards
     
    it "play a Draw and get a scored result" $ do
      play testFile `shouldBe` 4512
    it "convert a number to a scored number when it is found in the list" $ do
      checkNumber 17 (zip [1..20] [1..]) `shouldBe` (17,17) 
      checkNumber 21 (zip [1..20] [1..]) `shouldBe` (21, 0) 
      checkNumber 28 [(28,1), (10,2)] `shouldBe` (28, 1) 
    it "scores a Draw by matching the marked number and its called order" $ do
      score parsedBoard testDrawArray `shouldBe` scoredBoard      
    it "scores multiple Draw cards" $ do
      scoreDraw parsedBoards testDrawArray `shouldBe` scoredBoards
    it "should test a card to see if it is a Winning candidate" $ do
      isComplete scoredBoard `shouldBe` True
      isComplete scoredBoardNoLines `shouldBe` False
      isComplete scoredBoardColumnWin `shouldBe` True 
    it "should find a complete line row or col" $ do
      isCompleteLine [(1,1), (3,3), (99,2)] `shouldBe` True
      isCompleteLine [(1,0), (3,3), (99,2)] `shouldBe` False 
    it "should find the winning card with a completed row or column" $ do
      findWinner scoredBoards `shouldBe` scoredBoard
      findWinner [scoredBoardNoLines] `shouldBe` []
      findWinner [scoredBoardColumnWin] `shouldBe` scoredBoardColumnWin
      findWinner [[[(1,1)]]] `shouldBe` [[(1,1)]]
      findWinner [ [[(1,1)]], [[(2,0)]], [[(3,0)]] ] `shouldBe` [[(1,1)]]
      -- lets hope that is enough permutations  
    it "should calculate the winning score from the winning card" $ do
      scoreWinner [ [(1,1)], [(2,2)], [(3,0)] ] (2,2) `shouldBe` 6
      scoreWinner  scoredBoard (0,5) `shouldBe` 0
      scoreWinner scoredBoard  (1,5) `shouldBe` 237
      scoreWinner scoredBoardColumnWin  (1,5) `shouldBe` 242



