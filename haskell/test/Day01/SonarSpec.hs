module Day01.SonarSpec where

import Day01.Sonar
import Test.Hspec

spec :: Spec
spec = do
  describe "count increases" $ do
    it "1,2,3 increase two times" $ do
      countIncrease [1, 2, 3] `shouldBe` 2
