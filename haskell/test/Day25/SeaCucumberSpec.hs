{-# LANGUAGE OverloadedStrings #-}

module Day25.SeaCucumberSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Day25.SeaCucumber as SeaCucumber
import Test.Hspec

exampleInput :: [Text.Text]
exampleInput =
  [ "v...>>.vv>",
    ".vv>>.vv..",
    ">>.>v>...v",
    ">>v>>.>.v.",
    "v>v.vv.v..",
    ">.>>..v...",
    ".vv..>.>v.",
    "v.v..>>v.v",
    "....v..v.>"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "counts the steps until all sea cucumbers are stuck" $ do
      let cucumbers = SeaCucumber.parseInput exampleInput
      let steps = SeaCucumber.stepsUntilStuck cucumbers

      steps `shouldBe` 58

  describe "parse input" $ do
    it "parses a map of east and south moving sea cucubmers" $ do
      let SeaCucumber.Cucumbers {SeaCucumber.grid = cucumbers, SeaCucumber.bounds = (w, h)} = SeaCucumber.parseInput exampleInput

      (w, h) `shouldBe` (10, 9)

      Map.lookup (0, 0) cucumbers `shouldBe` Just SeaCucumber.South
      Map.lookup (1, 0) cucumbers `shouldBe` Nothing
      Map.lookup (4, 0) cucumbers `shouldBe` Just SeaCucumber.East
      Map.lookup (0, 3) cucumbers `shouldBe` Just SeaCucumber.East
      Map.lookup (0, 4) cucumbers `shouldBe` Just SeaCucumber.South
      Map.lookup (8, 8) cucumbers `shouldBe` Nothing
      Map.lookup (9, 8) cucumbers `shouldBe` Just SeaCucumber.East

  describe "move cucumbers" $ do
    it "simulates cucumbers moving east then south" $ do
      let input =
            SeaCucumber.parseInput
              [ "...>...",
                ".......",
                "......>",
                "v.....>",
                "......>",
                ".......",
                "..vvv.."
              ]
      let expected =
            SeaCucumber.parseInput
              [ "..vv>..",
                ".......",
                ">......",
                "v.....>",
                ">......",
                ".......",
                "....v.."
              ]

      (SeaCucumber.moveSouth . SeaCucumber.moveEast $ input) `shouldBe` expected
