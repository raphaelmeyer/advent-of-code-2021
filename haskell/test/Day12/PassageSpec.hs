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

spec :: Spec
spec = do
  describe "example" $ do
    let graph = Passage.parseInput exampleInput

    it "counts all paths" $ do
      let paths = Passage.searchPaths graph
      Passage.countPaths paths `shouldBe` 226

  describe "count paths" $ do
    it "counts paths in small example" $ do
      let graph = Passage.parseInput ["start-A", "start-b", "A-c", "A-b", "b-d", "A-end", "b-end"]
      let paths = Passage.searchPaths graph
      Passage.countPaths paths `shouldBe` 10

  describe "parse input" $ do
    it "parses the input" $ do
      let graph = Passage.parseInput exampleInput

      graph `shouldSatisfy` Map.member (Passage.Small "start")
      graph `shouldSatisfy` Map.member (Passage.Small "end")
      graph `shouldSatisfy` Map.member (Passage.Small "zg")
      graph `shouldSatisfy` Map.member (Passage.Big "RW")

      graph `shouldSatisfy` Map.member (Passage.Small "he")
      let he = graph Map.! Passage.Small "he"
      he `shouldSatisfy` Set.member (Passage.Big "WI")
      he `shouldSatisfy` Set.member (Passage.Big "DX")
      he `shouldSatisfy` Set.member (Passage.Small "fs")
