{-# LANGUAGE OverloadedStrings #-}

module Day09.LavaTubesSpec where

import qualified Data.Text as Text
import qualified Day09.LavaTubes as LavaTubes
import Test.Hspec

exampleInput :: [Text.Text]
exampleInput =
  [ "2199943210",
    "3987894921",
    "9856789892",
    "8767896789",
    "9899965678"
  ]

spec :: Spec
spec = do
  describe "parse input" $ do
    it "parses input grid" $ do
      let input = LavaTubes.parseInput ["12345", "67890", "02468", "97531"]
      input `shouldBe` [[1, 2, 3, 4, 5], [6, 7, 8, 9, 0], [0, 2, 4, 6, 8], [9, 7, 5, 3, 1]]

  describe "find low points" $ do
    it "returns a list of low points" $ do
      let grid = LavaTubes.parseInput exampleInput
      let lowPoints = LavaTubes.findLowPoints grid
      lowPoints `shouldMatchList` [0, 1, 5, 5]

  describe "example" $ do
    it "returns the risk level sum" $ do
      let grid = LavaTubes.parseInput exampleInput
      let riskLevelSum = LavaTubes.riskLevelSum grid
      riskLevelSum `shouldBe` 15
