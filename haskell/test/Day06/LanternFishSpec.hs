{-# LANGUAGE OverloadedStrings #-}

module Day06.LanternFishSpec where

import qualified Data.Text as Text
import qualified Day06.LanternFish as Fish
import Test.Hspec

exampleInput :: Text.Text
exampleInput = "3,4,3,1,2"

spec :: Spec
spec = do
  describe "simulate population" $ do
    it "simulates the population for a number of days" $ do
      Fish.simulate 18 exampleInput `shouldBe` 26
      Fish.simulate 80 exampleInput `shouldBe` 5934
      Fish.simulate 256 exampleInput `shouldBe` 26984457539

  describe "parse input" $ do
    it "count fishes by age" $ do
      Fish.parseInput exampleInput `shouldBe` [0, 1, 1, 2, 1, 0, 0, 0, 0]
