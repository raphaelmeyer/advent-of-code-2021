{-# LANGUAGE OverloadedStrings #-}

module Day12.PassageSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Day12.Passage as Passage
import Test.Hspec

exampleInput :: [Text.Text]
exampleInput =
  [ "fs-end",
    "he-DX",
    "fs-he",
    "start-DX",
    "pj-DX",
    "end-zg",
    "zg-sl",
    "zg-pj",
    "pj-he",
    "RW-he",
    "fs-DX",
    "pj-RW",
    "zg-RW",
    "start-pj",
    "he-WI",
    "zg-he",
    "pj-fs",
    "start-RW"
  ]

smallInput :: [Text.Text]
smallInput = ["start-A", "start-b", "A-c", "A-b", "b-d", "A-end", "b-end"]

spec :: Spec
spec = do
  describe "example" $ do
    let graph = Passage.parseInput exampleInput

    it "counts all paths" $ do
      Passage.countPaths graph `shouldBe` 226

    it "counts more paths" $ do
      Passage.countMorePaths graph `shouldBe` 3509

  describe "count paths" $ do
    it "counts paths in a small example" $ do
      let graph = Passage.parseInput smallInput
      Passage.countPaths graph `shouldBe` 10

  describe "count more paths" $ do
    it "counts more paths in a small example" $ do
      let graph = Passage.parseInput smallInput
      Passage.countMorePaths graph `shouldBe` 36

  describe "parse input" $ do
    it "parses the input" $ do
      let graph = Passage.parseInput exampleInput

      graph `shouldSatisfy` Map.member (Passage.Small "start")
      graph `shouldSatisfy` Map.notMember (Passage.Small "end")

      graph `shouldSatisfy` Map.member (Passage.Small "zg")
      let zg = graph Map.! Passage.Small "zg"
      zg `shouldSatisfy` Set.member (Passage.Small "end")

      graph `shouldSatisfy` Map.member (Passage.Big "RW")
      let rw = graph Map.! Passage.Big "RW"
      rw `shouldSatisfy` Set.notMember (Passage.Big "start")

      graph `shouldSatisfy` Map.member (Passage.Small "he")
      let he = graph Map.! Passage.Small "he"
      he `shouldSatisfy` Set.member (Passage.Big "WI")
      he `shouldSatisfy` Set.member (Passage.Big "DX")
      he `shouldSatisfy` Set.member (Passage.Small "fs")

      graph `shouldSatisfy` Map.notMember (Passage.Big "pj")
      graph `shouldSatisfy` Map.notMember (Passage.Small "DX")
