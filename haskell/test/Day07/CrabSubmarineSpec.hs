{-# LANGUAGE OverloadedStrings #-}

module Day07.CrabSubmarineSpec where

import qualified Data.Text as Text
import qualified Day07.CrabSubmarine as Crab
import Test.Hspec

exampleInput :: Text.Text
exampleInput = "16,1,2,0,4,2,7,1,2,14"

spec :: Spec
spec = do
  describe "calculate horizontal position with minimal fuel usage for constant rate" $ do
    it "returns fuel used if aligned at (any) median" $ do
      let input = Crab.parseInput exampleInput
      Crab.leastFuelConstant input `shouldBe` 37

  describe "calculate horizontal position with minimal fuel usage for increasing rate" $ do
    it "returns fuel used if aligned at mean ?" $ do
      let input = Crab.parseInput exampleInput
      Crab.leastFuelIncreasing input `shouldBe` 168
