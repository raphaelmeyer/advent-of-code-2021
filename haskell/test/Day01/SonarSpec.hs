module Day01.SonarSpec where

import Day01.Sonar
import Test.Hspec

spec :: Spec
spec = do
  describe "count increases" $ do
    it "1,2,3 increase two times" $ do
      countIncrease [1, 2, 3] `shouldBe` 2

    it "example input" $ do
      countIncrease [199,200,208,210,200,207,240,269,260,263] `shouldBe` 7

  describe "3 value sliding window sum" $ do
    it "should return the sum of each sliding window of size 3" $ do
      windowSums [1,10,100,20,2,400] `shouldBe` [111,130,122,422]

    it "example input" $ do
      windowSums [199,200,208,210,200,207,240,269,260,263] `shouldBe` [607,618,618,617,647,716,769,792]
