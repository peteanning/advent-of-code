module Main where

import Test.Hspec

main = hspec $ do
  describe "Vanilla Test" $ do
    it "Foo should be true" $ do
      1 == 1 `shouldBe` True

