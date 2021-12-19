{-# LANGUAGE OverloadedStrings #-}

module Day18.SnailfishSpec where

import qualified Data.Text as Text
import qualified Day18.Number as S
import qualified Day18.Parser as Snailfish
import qualified Day18.Snailfish as Snailfish
import Test.Hspec

exampleInput :: [Text.Text]
exampleInput =
  [ "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]",
    "[[[5,[2,8]],4],[5,[[9,9],0]]]",
    "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]",
    "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]",
    "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]",
    "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]",
    "[[[[5,4],[7,7]],8],[[8,3],8]]",
    "[[9,3],[[9,9],[6,[4,9]]]]",
    "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]",
    "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"
  ]

otherInput :: [Text.Text]
otherInput =
  [ "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]",
    "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]",
    "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]",
    "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]",
    "[7,[5,[[3,8],[1,4]]]]",
    "[[2,[2,2]],[8,[8,1]]]",
    "[2,9]",
    "[1,[[[9,3],9],[[9,0],[0,7]]]]",
    "[[[5,[7,4]],7],1]",
    "[[[[4,2],2],6],[8,7]]"
  ]

spec :: Spec
spec = do
  describe "example part 1" $ do
    it "calculates the magnitude of the sum" $ do
      let homework = Snailfish.parseInput exampleInput
      let result = Snailfish.sum homework
      let magnitude = Snailfish.magnitude result

      magnitude `shouldBe` 4140

    it "calculates the magnitude of another example" $ do
      let homework = Snailfish.parseInput otherInput
      let result = Snailfish.sum homework
      let magnitude = Snailfish.magnitude result

      magnitude `shouldBe` 3488

  describe "example part 2" $ do
    it "calculates the maximum magnitude of any pair added" $ do
      let homework = Snailfish.parseInput exampleInput
      let maxMagnitude = Snailfish.maxMagnitude homework

      maxMagnitude `shouldBe` 3993

  describe "calculate magnitude" $ do
    it "calculates the magnitude" $ do
      let magnitude = Snailfish.magnitude . Snailfish.parseNumber
      magnitude "[[1,2],[[3,4],5]]" `shouldBe` 143
      magnitude "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]" `shouldBe` 1384
      magnitude "[[[[1,1],[2,2]],[3,3]],[4,4]]" `shouldBe` 445
      magnitude "[[[[3,0],[5,3]],[4,4]],[5,5]]" `shouldBe` 791
      magnitude "[[[[5,0],[7,4]],[5,5]],[6,6]]" `shouldBe` 1137
      magnitude "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]" `shouldBe` 3488

  describe "add up of snailfish numbers" $ do
    it "adds up a list numbers" $ do
      let homework = Snailfish.parseInput exampleInput
      let result = Snailfish.sum homework
      let expected = Snailfish.parseNumber "[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]"

      result `shouldBe` expected

    it "another example" $ do
      let homework = Snailfish.parseInput otherInput
      let result = Snailfish.sum homework
      let expected = Snailfish.parseNumber "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"

      result `shouldBe` expected

  describe "reduce" $ do
    it "reduces a snailfish number" $ do
      let number = Snailfish.parseNumber "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"
      let expected = Snailfish.parseNumber "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"
      Snailfish.reduce number `shouldBe` expected

  describe "explode a snailfish number" $ do
    it "no left" $ do
      let number = Snailfish.parseNumber "[[[[[9,8],1],2],3],4]"
      let expected = Snailfish.parseNumber "[[[[0,9],2],3],4]"
      Snailfish.explode number `shouldBe` Just expected

    it "no right" $ do
      let number = Snailfish.parseNumber "[7,[6,[5,[4,[3,2]]]]]"
      let expected = Snailfish.parseNumber "[7,[6,[5,[7,0]]]]"
      Snailfish.explode number `shouldBe` Just expected

    it "left and right" $ do
      let number = Snailfish.parseNumber "[[6,[5,[4,[3,2]]]],1]"
      let expected = Snailfish.parseNumber "[[6,[5,[7,0]]],3]"
      Snailfish.explode number `shouldBe` Just expected

    it "one at a time" $ do
      let num = Snailfish.parseNumber "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"
      let expectedFirst = Snailfish.parseNumber "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"

      let first = Snailfish.explode num
      first `shouldBe` Just expectedFirst

      let expectedSecond = Snailfish.parseNumber "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"
      let second = first >>= Snailfish.explode
      second `shouldBe` Just expectedSecond

    it "returns nothing when there's no explosion" $ do
      let number = Snailfish.parseNumber "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"
      Snailfish.explode number `shouldBe` Nothing

  describe "split a snailfish number" $ do
    it "splits a number" $ do
      let number = Snailfish.parseNumber "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]"
      let expected = Snailfish.parseNumber "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]"
      Snailfish.split number `shouldBe` Just expected

    it "splits left most first" $ do
      let number = Snailfish.parseNumber "[[[[0,7],4],[15,[0,13]]],[1,1]]"
      let expected = Snailfish.parseNumber "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]"
      Snailfish.split number `shouldBe` Just expected

    it "returns nothing when there's nothing to split" $ do
      let number = Snailfish.parseNumber "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]"
      Snailfish.split number `shouldBe` Nothing

  describe "parse input" $ do
    it "parses a snailfish number" $ do
      Snailfish.parseNumber (exampleInput !! 1)
        `shouldBe` S.Pair (S.Pair (S.Pair (S.Regular 5) (S.Pair (S.Regular 2) (S.Regular 8))) (S.Regular 4)) (S.Pair (S.Regular 5) (S.Pair (S.Pair (S.Regular 9) (S.Regular 9)) (S.Regular 0)))

      Snailfish.parseNumber "[[[[0,9],2],3],4]"
        `shouldBe` S.Pair (S.Pair (S.Pair (S.Pair (S.Regular 0) (S.Regular 9)) (S.Regular 2)) (S.Regular 3)) (S.Regular 4)

      Snailfish.parseNumber "[7,[6,[5,[7,0]]]]"
        `shouldBe` S.Pair (S.Regular 7) (S.Pair (S.Regular 6) (S.Pair (S.Regular 5) (S.Pair (S.Regular 7) (S.Regular 0))))
