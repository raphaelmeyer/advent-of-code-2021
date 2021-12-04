{-# LANGUAGE OverloadedStrings #-}

module Day03.DiagnosticSpec where

import qualified Data.Text as Text
import Day03.Diagnostic
import Test.Hspec

exampleInput :: [Text.Text]
exampleInput =
  [ "00100",
    "11110",
    "10110",
    "10101",
    "10111",
    "01111",
    "00111",
    "11100",
    "10000",
    "11001",
    "00010",
    "01010"
  ]

spec :: Spec
spec = do
  describe "power rate" $ do
    it "is product of gamma and epsilon" $ do
      powerRate exampleInput `shouldBe` 198

  describe "gamma and epsilon rates" $ do
    it "calculates gamma and epsilon rate" $ do
      calcuateRates exampleInput `shouldBe` (22, 9)

  describe "bit list" $ do
    it "converts bit string to bit list" $ do
      toBitList "10" `shouldBe` [1, 0]
      toBitList "01" `shouldBe` [0, 1]
      toBitList "10110" `shouldBe` [1, 0, 1, 1, 0]

  describe "sum up bits" $ do
    it "sums up all bits in same position" $ do
      sumUpBits [[1, 0, 0], [1, 1, 0]] `shouldBe` [2, 1, 0]
      sumUpBits [[1, 1, 0, 1], [1, 0, 0, 0], [1, 1, 0, 0]] `shouldBe` [3, 2, 0, 1]

  describe "most or least common bits" $ do
    it "decide if bit is one or zero and convert to int" $ do
      mostCommonInt [1, 4, 3, 2] 2 `shouldBe` (6, 9) -- 0110 , 1001
      mostCommonInt [5, 1, 2, 3, 4] 2 `shouldBe` (19, 12) -- 10011 , 01100

  -- part 2

  describe "life support" $ do
    it "is product of oxygen and CO2 scrubber rating" $ do
      lifeSupportRating exampleInput `shouldBe` 230

  describe "build tree" $ do
    it "add bit list to empty tree" $ do
      insertBits Nil [1, 0, 1] `shouldBe` Node (0, 1) Nil (Node (1, 0) (Node (0, 1) Nil Nil) Nil)

    it "add bit list to tree" $ do
      let tree = Node (1, 1) (Node (0, 1) Nil (Node (1, 0) Nil Nil)) (Node (1, 0) (Node (0, 1) Nil Nil) Nil)
      let expected = Node (2, 1) (Node (0, 2) Nil (Node (1, 1) Nil Nil)) (Node (1, 0) (Node (0, 1) Nil Nil) Nil)
      insertBits tree [0, 1, 1] `shouldBe` expected

  describe "count oxygen rating" $ do
    it "follows most common bits" $ do
      let lists = map toBitList exampleInput
      let tree = foldl insertBits Nil lists
      countOxygen tree `shouldBe` 23

  describe "count CO2 scrubber rating" $ do
    it "follows least common bits" $ do
      let lists = map toBitList exampleInput
      let tree = foldl insertBits Nil lists
      countCO2Scrubs tree `shouldBe` 10
