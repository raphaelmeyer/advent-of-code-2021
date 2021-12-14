{-# LANGUAGE OverloadedStrings #-}

module Day13.OrigamiSpec where

import qualified Data.Text as Text
import Day13.Origami (Fold (FoldY))
import qualified Day13.Origami as Origami
import Test.Hspec

exampleInput :: [Text.Text]
exampleInput =
  [ "6,10",
    "0,14",
    "9,10",
    "0,3",
    "10,4",
    "4,11",
    "6,0",
    "6,12",
    "4,1",
    "0,13",
    "10,12",
    "3,4",
    "3,0",
    "8,4",
    "1,10",
    "2,14",
    "8,10",
    "9,0",
    "",
    "fold along y=7",
    "fold along x=5"
  ]

spec :: Spec
spec = do
  describe "example" $ do
    let (paper, instructions) = Origami.parseInput exampleInput

    it "counts dots after first fold" $ do
      let dots = Origami.countDots . Origami.fold paper . head $ instructions
      dots `shouldBe` 17

    it "folds according to instructions" $ do
      let folded = Origami.finish paper instructions

      length folded `shouldBe` 16

      folded `shouldContain` [(0, 0)]
      folded `shouldContain` [(1, 0)]
      folded `shouldContain` [(2, 0)]
      folded `shouldContain` [(3, 0)]
      folded `shouldContain` [(4, 0)]

      folded `shouldContain` [(0, 1)]
      folded `shouldContain` [(0, 2)]
      folded `shouldContain` [(0, 3)]
      folded `shouldContain` [(0, 4)]

      folded `shouldContain` [(4, 2)]
      folded `shouldContain` [(4, 3)]

      folded `shouldContain` [(2, 4)]
      folded `shouldContain` [(3, 4)]
      folded `shouldContain` [(4, 4)]

  describe "parse input" $ do
    let (paper, instructions) = Origami.parseInput exampleInput

    it "parses the dots on the paper" $ do
      length paper `shouldBe` 18

      paper `shouldContain` [(6, 10)]
      paper `shouldContain` [(0, 13)]
      paper `shouldContain` [(8, 4)]

    it "parses the folding instructions" $ do
      length instructions `shouldBe` 2

      instructions `shouldBe` [Origami.FoldY 7, Origami.FoldX 5]

  describe "fold" $ do
    it "fold once" $ do
      let (paper, _) = Origami.parseInput exampleInput
      let folded = Origami.fold paper (FoldY 7)

      folded `shouldContain` [(0, 0)]
      folded `shouldContain` [(8, 4)]
