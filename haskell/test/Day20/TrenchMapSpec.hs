{-# LANGUAGE OverloadedStrings #-}

module Day20.TrenchMapSpec where

import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Day20.Grid as Grid
import qualified Day20.TrenchMap as TrenchMap
import Test.Hspec

exampleInput :: [Text.Text]
exampleInput =
  [ Text.concat
      [ "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##",
        "#..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###",
        ".######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#.",
        ".#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#.....",
        ".#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#..",
        "...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.....",
        "..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#"
      ],
    "",
    "#..#.",
    "#....",
    "##..#",
    "..#..",
    "..###"
  ]

step1 :: [Text.Text]
step1 =
  [ ".##.##.",
    "#..#.#.",
    "##.#..#",
    "####..#",
    ".#..##.",
    "..##..#",
    "...#.#."
  ]

step2 :: [Text.Text]
step2 =
  [ ".......#.",
    ".#..#.#..",
    "#.#...###",
    "#...##.#.",
    "#.....#.#",
    ".#.#####.",
    "..#.#####",
    "...##.##.",
    "....###.."
  ]

spec :: Spec
spec = do
  describe "example part 1" $ do
    it "count lit pixels after enhancing twice" $ do
      let (algo, image) = TrenchMap.parseInput exampleInput

      let enhanced = TrenchMap.enhance image algo
      let lightPixels = TrenchMap.countLightPixels enhanced

      lightPixels `shouldBe` 35

  describe "example part 2" $ do
    it "count lit pixels after enhancing 50x" $ do
      let (algo, image) = TrenchMap.parseInput exampleInput

      let enhanced = iterate (`TrenchMap.enhance` algo) image !! 25
      let lightPixels = TrenchMap.countLightPixels enhanced

      lightPixels `shouldBe` 3351

  describe "enhance image" $ do
    it "enhances the image with the given algorithm" $ do
      let (algo, image) = TrenchMap.parseInput exampleInput
      let expected = TrenchMap.parseImage step2

      let enhanced = TrenchMap.enhance image algo

      enhanced `shouldBe` expected

  describe "parse input" $ do
    it "parses the algorithm" $ do
      let (algo, _) = TrenchMap.parseInput exampleInput

      Vector.length algo `shouldBe` 512

      algo Vector.! 0 `shouldBe` TrenchMap.Dark
      algo Vector.! 2 `shouldBe` TrenchMap.Light
      algo Vector.! 3 `shouldBe` TrenchMap.Dark
      algo Vector.! 510 `shouldBe` TrenchMap.Dark
      algo Vector.! 511 `shouldBe` TrenchMap.Light

    it "parses the input image" $ do
      let (_, image) = TrenchMap.parseInput exampleInput

      length image `shouldBe` 5
      map length image `shouldSatisfy` all (== 5)

      (image !! 1) `shouldBe` [TrenchMap.Light, TrenchMap.Dark, TrenchMap.Dark, TrenchMap.Dark, TrenchMap.Dark]
      (image !! 4) `shouldBe` [TrenchMap.Dark, TrenchMap.Dark, TrenchMap.Light, TrenchMap.Light, TrenchMap.Light]

  describe "count lit pixels" $ do
    it "counts all lit pixels" $ do
      let (_, image) = TrenchMap.parseInput exampleInput

      TrenchMap.countLightPixels image `shouldBe` 10

  describe "traverse and get neighbors" $ do
    let (_, image) = TrenchMap.parseInput exampleInput
    let view = Grid.gridFromList image
    let advance = iterate Grid.advance view

    it "returns an ordered list of neighbors at the current location" $ do
      Grid.neighbors view
        `shouldBe` [Nothing, Nothing, Nothing]
        ++ [Nothing, Just TrenchMap.Light, Just TrenchMap.Dark]
        ++ [Nothing, Just TrenchMap.Light, Just TrenchMap.Dark]

      Grid.neighbors (Grid.advance view)
        `shouldBe` [Nothing, Nothing, Nothing]
        ++ [Just TrenchMap.Light, Just TrenchMap.Dark, Just TrenchMap.Dark]
        ++ [Just TrenchMap.Light, Just TrenchMap.Dark, Just TrenchMap.Dark]

      Grid.neighbors (advance !! 4)
        `shouldBe` [Nothing, Nothing, Nothing]
        ++ [Just TrenchMap.Light, Just TrenchMap.Dark, Nothing]
        ++ [Just TrenchMap.Dark, Just TrenchMap.Dark, Nothing]

      Grid.neighbors (advance !! 5)
        `shouldBe` [Nothing, Just TrenchMap.Light, Just TrenchMap.Dark]
        ++ [Nothing, Just TrenchMap.Light, Just TrenchMap.Dark]
        ++ [Nothing, Just TrenchMap.Light, Just TrenchMap.Light]

      Grid.neighbors (advance !! 12)
        `shouldBe` [Just TrenchMap.Dark, Just TrenchMap.Dark, Just TrenchMap.Dark]
        ++ [Just TrenchMap.Light, Just TrenchMap.Dark, Just TrenchMap.Dark]
        ++ [Just TrenchMap.Dark, Just TrenchMap.Light, Just TrenchMap.Dark]

      Grid.neighbors (advance !! 24)
        `shouldBe` [Just TrenchMap.Dark, Just TrenchMap.Dark, Nothing]
        ++ [Just TrenchMap.Light, Just TrenchMap.Light, Nothing]
        ++ [Nothing, Nothing, Nothing]

    it "returns GridEnd after advancing from last pixel" $ do
      (advance !! 25) `shouldBe` Grid.GridEnd
      (advance !! 27) `shouldBe` Grid.GridEnd

  describe "convert neighbors to binary algorithm index" $ do
    it "calculates the binary from neighbors" $ do
      let neighbors =
            [Just TrenchMap.Dark, Just TrenchMap.Dark, Just TrenchMap.Light]
              ++ [Just TrenchMap.Light, Just TrenchMap.Dark, Just TrenchMap.Dark]
              ++ [Just TrenchMap.Dark, Just TrenchMap.Light, Just TrenchMap.Dark]

      -- 001100010
      TrenchMap.binary TrenchMap.Dark neighbors `shouldBe` (64 + 32 + 2)

    it "calculates binary when outside is dark" $ do
      let neighbors =
            [Nothing, Nothing, Nothing]
              ++ [Just TrenchMap.Light, Just TrenchMap.Dark, Just TrenchMap.Dark]
              ++ [Just TrenchMap.Light, Just TrenchMap.Dark, Just TrenchMap.Dark]

      -- 000100100
      TrenchMap.binary TrenchMap.Dark neighbors `shouldBe` (32 + 4)

    it "calculates binary when outside is light" $ do
      let neighbors =
            [Just TrenchMap.Light, Just TrenchMap.Dark, Just TrenchMap.Dark]
              ++ [Just TrenchMap.Light, Just TrenchMap.Dark, Just TrenchMap.Dark]
              ++ [Nothing, Nothing, Nothing]

      -- 100100111
      TrenchMap.binary TrenchMap.Light neighbors `shouldBe` (256 + 32 + 4 + 2 + 1)

  describe "add border" $ do
    it "adds a border around grid" $ do
      TrenchMap.border TrenchMap.Light [[TrenchMap.Dark, TrenchMap.Light]]
        `shouldBe` [ [TrenchMap.Light, TrenchMap.Light, TrenchMap.Light, TrenchMap.Light],
                     [TrenchMap.Light, TrenchMap.Dark, TrenchMap.Light, TrenchMap.Light],
                     [TrenchMap.Light, TrenchMap.Light, TrenchMap.Light, TrenchMap.Light]
                   ]

      TrenchMap.border TrenchMap.Dark [[TrenchMap.Light], [TrenchMap.Dark]]
        `shouldBe` [ [TrenchMap.Dark, TrenchMap.Dark, TrenchMap.Dark],
                     [TrenchMap.Dark, TrenchMap.Light, TrenchMap.Dark],
                     [TrenchMap.Dark, TrenchMap.Dark, TrenchMap.Dark],
                     [TrenchMap.Dark, TrenchMap.Dark, TrenchMap.Dark]
                   ]
