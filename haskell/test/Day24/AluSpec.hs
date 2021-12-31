{-# LANGUAGE OverloadedStrings #-}

module Day24.AluSpec where

-- import qualified Data.Text as Text
import qualified Day24.Alu as Alu
import Test.Hspec

spec :: Spec
spec = do
  describe "part 1" $ do
    it "calculates the maximum serial number" $ do
      input <- Alu.readInput "data/day-24.txt"
      let parameters = Alu.parseInput input
      Alu.maxSerial parameters `shouldBe` "79197919993985"

  describe "part 2" $ do
    it "calculates the minimum serial number" $ do
      input <- Alu.readInput "data/day-24.txt"
      let parameters = Alu.parseInput input
      Alu.minSerial parameters `shouldBe` "13191913571211"

  describe "parse input" $ do
    it "counts cubes that are turned on" $ do
      input <- Alu.readInput "data/day-24.txt"
      let parameters = Alu.parseInput input

      length parameters `shouldBe` 14
      parameters
        `shouldBe` [ Alu.Parameters {Alu.getDivZ = False, Alu.getAddX = 12, Alu.getAddY = 7},
                     Alu.Parameters {Alu.getDivZ = False, Alu.getAddX = 13, Alu.getAddY = 8},
                     Alu.Parameters {Alu.getDivZ = False, Alu.getAddX = 13, Alu.getAddY = 10},
                     Alu.Parameters {Alu.getDivZ = True, Alu.getAddX = -2, Alu.getAddY = 4},
                     Alu.Parameters {Alu.getDivZ = True, Alu.getAddX = -10, Alu.getAddY = 4},
                     Alu.Parameters {Alu.getDivZ = False, Alu.getAddX = 13, Alu.getAddY = 6},
                     Alu.Parameters {Alu.getDivZ = True, Alu.getAddX = -14, Alu.getAddY = 11},
                     Alu.Parameters {Alu.getDivZ = True, Alu.getAddX = -5, Alu.getAddY = 13},
                     Alu.Parameters {Alu.getDivZ = False, Alu.getAddX = 15, Alu.getAddY = 1},
                     Alu.Parameters {Alu.getDivZ = False, Alu.getAddX = 15, Alu.getAddY = 8},
                     Alu.Parameters {Alu.getDivZ = True, Alu.getAddX = -14, Alu.getAddY = 4},
                     Alu.Parameters {Alu.getDivZ = False, Alu.getAddX = 10, Alu.getAddY = 13},
                     Alu.Parameters {Alu.getDivZ = True, Alu.getAddX = -14, Alu.getAddY = 4},
                     Alu.Parameters {Alu.getDivZ = True, Alu.getAddX = -5, Alu.getAddY = 14}
                   ]

  describe "verify valid serial number" $ do
    it "checks if a serial number is valid" $ do
      input <- Alu.readInput "data/day-24.txt"
      let parameters = Alu.parseInput input

      [7, 9, 1, 9, 7, 9, 1, 9, 9, 9, 3, 9, 8, 5] `shouldSatisfy` (parameters `Alu.verify`)
      [1, 3, 1, 9, 1, 9, 1, 3, 5, 7, 1, 2, 1, 1] `shouldSatisfy` (parameters `Alu.verify`)

      [1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 3, 5, 7, 9] `shouldNotSatisfy` (parameters `Alu.verify`)