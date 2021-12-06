{-# LANGUAGE OverloadedStrings #-}

module Day05.VentsSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Day05.Vents
import Test.Hspec

exampleInput :: [Text.Text]
exampleInput =
  [ "0,9 -> 5,9",
    "8,0 -> 0,8",
    "9,4 -> 3,4",
    "2,2 -> 2,1",
    "7,0 -> 7,4",
    "6,4 -> 2,0",
    "0,9 -> 2,9",
    "3,4 -> 1,4",
    "0,0 -> 8,8",
    "5,5 -> 8,2"
  ]

spec :: Spec
spec = do
  describe "parse input" $ do
    it "parses from and to of a line" $ do
      parseInput ["9,4 -> 3,4"] `shouldBe` [Line (Position 9 4) (Position 3 4)]
      parseInput ["7,0 -> 7,4", "6,4 -> 2,0"] `shouldBe` [Line (Position 7 0) (Position 7 4), Line (Position 6 4) (Position 2 0)]

  describe "danger" $ do
    it "count points where at least two vents interleave" $ do
      dangerSimple exampleInput `shouldBe` 5

    it "also counts diagonal lines" $ do
      danger exampleInput `shouldBe` 12

  describe "consider only horizontal and vertical lines" $ do
    it "removes slant lines" $ do
      let input = parseInput ["9,4 -> 3,4", "8,0 -> 0,8", "2,2 -> 2,1"]
      removeSlant input `shouldBe` [Line (Position 9 4) (Position 3 4), Line (Position 2 2) (Position 2 1)]

  describe "create diagram" $ do
    it "counts vents in positions" $ do
      let input = parseInput exampleInput
      let diagram = createDiagram input

      diagram Map.!? Position 3 2 `shouldBe` Nothing
      diagram Map.!? Position 2 2 `shouldBe` Just 2
      diagram Map.!? Position 6 4 `shouldBe` Just 3

  describe "find positions lines" $ do
    it "finds positions on straight lines" $ do
      findPositions (Line (Position 5 11) (Position 7 11)) `shouldMatchList` [Position 5 11, Position 6 11, Position 7 11]
      findPositions (Line (Position 4 5) (Position 4 8)) `shouldMatchList` [Position 4 5, Position 4 6, Position 4 7, Position 4 8]
      findPositions (Line (Position 12 5) (Position 10 5)) `shouldMatchList` [Position 10 5, Position 11 5, Position 12 5]
      findPositions (Line (Position 23 17) (Position 23 14)) `shouldMatchList` [Position 23 14, Position 23 15, Position 23 16, Position 23 17]

    it "finds positions on diagonal lines" $ do
      findPositions (Line (Position 3 1) (Position 5 3)) `shouldMatchList` [Position 3 1, Position 4 2, Position 5 3]
      findPositions (Line (Position 7 2) (Position 5 4)) `shouldMatchList` [Position 7 2, Position 6 3, Position 5 4]
      findPositions (Line (Position 8 11) (Position 6 9)) `shouldMatchList` [Position 8 11, Position 7 10, Position 6 9]
      findPositions (Line (Position 4 7) (Position 6 5)) `shouldMatchList` [Position 6 5, Position 5 6, Position 4 7]

  describe "find danger" $ do
    it "finds all positions with at least two vents" $ do
      let input = parseInput ["7,2 -> 7,4", "11,4 -> 6,4", "11,4 -> 12,4"]
      let diagram = createDiagram input
      let dangerous = findDanger diagram
      length dangerous `shouldBe` 2
      dangerous `shouldContain` [Position 7 4, Position 11 4]
