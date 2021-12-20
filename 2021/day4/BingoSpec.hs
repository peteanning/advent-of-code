module BingoSpece where

import Test.Hspec

import Bingo

testFile :: String
testFile = "7,4,9,5,11,17,23\n\n22 13 17 11  0\n 8  2 23  4 24\n21  9 14 16  7\n 6 10  3 18  5\n 1 12 20 15 19\n\n" 
testgame = "7,4,9,5,11,17,23"
testgameStrArray = ["7","4","9","5","11","17","23"]
cards = ["", "22 13 17 11  0"," 8  2 23  4 24","21  9 14 16  7"," 6 10  3 18  5"," 1 12 20 15 19",""]
testgameInts = [7,4,9,5,11,17,23] :: [Int]
parsedCards :: [[[Int]]]
parsedCards =[ [ [22, 13, 17, 11,  0],[ 8,  2, 23,  4, 24],[21,  9, 14, 16,  7],[ 6, 10,  3, 18,  5],[ 1, 12, 20, 15, 19] ]]


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





