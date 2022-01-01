{-# LANGUAGE OverloadedStrings #-}

module Day24.AluSpec where

import qualified Day24.Alu as Alu
import Test.Hspec

spec :: Spec
spec = do
  describe "part 1" $ do
    it "calculates the maximum serial number" $ do
      input <- Alu.readInput "data/day-24.txt"
      let actions = Alu.parseInput input
      Alu.maxSerial actions `shouldBe` "79197919993985"

  describe "part 2" $ do
    it "calculates the minimum serial number" $ do
      input <- Alu.readInput "data/day-24.txt"
      let actions = Alu.parseInput input
      Alu.minSerial actions `shouldBe` "13191913571211"

  describe "parse input" $ do
    it "extract actions from instructions" $ do
      input <- Alu.readInput "data/day-24.txt"
      let actions = Alu.parseInput input

      length actions `shouldBe` 14
      actions
        `shouldBe` [ Alu.Push 7,
                     Alu.Push 8,
                     Alu.Push 10,
                     Alu.Pop (-2),
                     Alu.Pop (-10),
                     Alu.Push 6,
                     Alu.Pop (-14),
                     Alu.Pop (-5),
                     Alu.Push 1,
                     Alu.Push 8,
                     Alu.Pop (-14),
                     Alu.Push 13,
                     Alu.Pop (-14),
                     Alu.Pop (-5)
                   ]
