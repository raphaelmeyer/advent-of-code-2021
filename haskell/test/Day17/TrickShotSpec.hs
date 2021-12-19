{-# LANGUAGE OverloadedStrings #-}

module Day17.TrickShotSpec where

import qualified Data.Text as Text
import qualified Day17.TrickShot as TrickShot
import Test.Hspec

exampleInput :: Text.Text
exampleInput = "target area: x=20..30, y=-10..-5"

spec :: Spec
spec = do
  describe "example" $ do
    it "find trajectories" $ do
      let area = TrickShot.parseInput exampleInput
      let trajectory = TrickShot.calculateTrajectories area
      TrickShot.getMaxY trajectory `shouldBe` 45
      TrickShot.getHits trajectory `shouldBe` 112

  describe "parse input" $ do
    it "parses the input" $ do
      TrickShot.parseInput exampleInput `shouldBe` TrickShot.Area (20, 30) (-10, -5)

  describe "shoot" $ do
    it "calculates a trajectory" $ do
      let target = TrickShot.Area (20, 30) (-10, -5)
      let start = (0, 0)
      TrickShot.shoot start (7, 2) target 0 `shouldBe` Just 3
      TrickShot.shoot start (6, 3) target 0 `shouldBe` Just 6
      TrickShot.shoot start (9, 0) target 0 `shouldBe` Just 0
      TrickShot.shoot start (6, 0) target 0 `shouldBe` Just 0
      TrickShot.shoot start (7, -1) target 0 `shouldBe` Just 0
      TrickShot.shoot start (17, -4) target 0 `shouldBe` Nothing
