{-# LANGUAGE OverloadedStrings #-}

module Day16.BitsSpec where

import qualified Data.Text as Text
import qualified Day16.Bits as Bits
import Test.Hspec

examples :: [Text.Text]
examples =
  [ "D2FE28",
    "8A004A801A8002F478",
    "620080001611562C8802118E34",
    "C0015000016115A2E0802F182340",
    "A0016C880162017C3686B18A3D4780"
  ]

spec :: Spec
spec = do
  describe "example" $ do
    it "adds up all version numbers from packet and its nested packets" $ do
      let sumerizer = Bits.versionSum . Bits.decode . Bits.parseInput
      sumerizer (head examples) `shouldBe` 6
      sumerizer (examples !! 1) `shouldBe` 16
      sumerizer (examples !! 2) `shouldBe` 12
      sumerizer (examples !! 3) `shouldBe` 23
      sumerizer (examples !! 4) `shouldBe` 31

  describe "parse input" $ do
    it "converts the transmission to binary" $ do
      let (_0, _1) = (Bits.Zero, Bits.One)
      let transmission = Bits.parseInput . head $ examples
      transmission `shouldBe` [_1, _1, _0, _1, _0, _0, _1, _0, _1, _1, _1, _1, _1, _1, _1, _0, _0, _0, _1, _0, _1, _0, _0, _0]

  describe "decoding packets" $ do
    it "decodes a literal packet" $ do
      (Bits.decode . Bits.parseInput $ "D2FE28") `shouldBe` Bits.Literal 6 2021

    it "decodes an operator packet of type 0" $ do
      (Bits.decode . Bits.parseInput $ "38006F45291200") `shouldBe` Bits.Operator 1 [Bits.Literal 6 10, Bits.Literal 2 20]

    it "decodes an operator packet of type 1" $ do
      (Bits.decode . Bits.parseInput $ "EE00D40C823060") `shouldBe` Bits.Operator 7 [Bits.Literal 2 1, Bits.Literal 4 2, Bits.Literal 1 3]

  describe "value of binary number" $ do
    it "converts binary to number" $ do
      Bits.value [Bits.One, Bits.Zero, Bits.One, Bits.One] `shouldBe` 11
      Bits.value [Bits.One, Bits.One, Bits.One, Bits.Zero, Bits.Zero] `shouldBe` 28
