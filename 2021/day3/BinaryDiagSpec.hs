module BinaryDiagSpec where

import Test.Hspec

import BinaryDiag

main :: IO ()
main = hspec $ do
  describe "Binary Diagnostic Tests" $ do
    it "Convert Empty String to Binary" $ do
      toWord "" `shouldBe` [Zero]
    it "Convert 11111 to (One, One, One, One, One)" $ do
      toWord "11111" `shouldBe` [One, One, One, One, One]
    it "Convert 00000 to (Zero, Zero, Zero, Zero, Zero)" $ do
      toWord "00000" `shouldBe` [Zero, Zero, Zero, Zero, Zero]
    it "Convert 01010 to (Zero, One, Zero, One, Zero)" $ do
      toWord "01010" `shouldBe` [Zero, One, Zero, One, Zero]
    
    it "Convert Zero to 0" $ do
      bin2Int Zero `shouldBe` 0
    it "Convert One to 1" $ do
      bin2Int One `shouldBe` 1

    it "Converts a count of the occurrance of Binary Ones and a number of words that produced the count and computes the Winner Zero or One in as a 5 bit binary word" $ do
        countToWord 2 [2,2] `shouldBe` [One, One]
        countToWord 2 [1,2] `shouldBe` [Zero , One]
        countToWord 12 [5,7] `shouldBe` [Zero , One]

    it "Sum up digits in Int arrays" $ do
        sumInt [1,2,3,4] [4,3,2,1]  `shouldBe` [5,5,5,5]
        sumInt [1,1,2,5] [1,2,1,3] `shouldBe` [2,3,3,8] 

    it "Count the occurance of the most common bit in each position of a Binary Word" $ do
        countOnes [[One,Zero], [One, Zero], [One, One], [One, Zero]] `shouldBe` [One,Zero]

    it "Convert a Binary Array of [One, Zero, One, Zero, One] to decimal 22" $ do
       binaryArrayToInt [One, Zero, One, One, Zero ] `shouldBe` 22

    it "Bitflip a Binary Array so One becomes Zero and Zero becomes One in [One, Zero] => [Zero, One]" $ do
      bitflip [One, Zero, One, Zero] `shouldBe` [Zero, One, Zero, One]


