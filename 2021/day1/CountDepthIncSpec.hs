module CountDepthIncSpec where
import Test.Hspec
import CountDepthInc

main :: IO ()
main = hspec $ do
  describe "count depth increase" $ do
    it "should be increase of 0 with an empty array" $
     getIncreases [] 0 `shouldBe` 0
    it "should be increase of 0 with one depth" $
     getIncreases [100] 0 `shouldBe` 0
    it "should be increase of 0 with no increase in depth" $
     getIncreases [100,99,98,10,9,8,7,6] 0 `shouldBe` 0
    it "should be increase of 5 with 5 increases" $
     getIncreases [10,11,12,11,10,9,12,13,14] 0 `shouldBe` 5


