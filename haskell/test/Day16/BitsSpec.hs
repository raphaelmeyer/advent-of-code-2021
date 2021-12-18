{-# LANGUAGE OverloadedStrings #-}

module Day16.BitsSpec where

import qualified Day16.Bits as Bits
import Test.Hspec

spec :: Spec
spec = do
  describe "example" $ do
    it "adds up all version numbers from packet and its nested packets" $ do
      let sumerizer = Bits.versionSum . Bits.decode . Bits.parseInput
      sumerizer "D2FE28" `shouldBe` 6
      sumerizer "8A004A801A8002F478" `shouldBe` 16
      sumerizer "620080001611562C8802118E34" `shouldBe` 12
      sumerizer "C0015000016115A2E0802F182340" `shouldBe` 23
      sumerizer "A0016C880162017C3686B18A3D4780" `shouldBe` 31

    it "calculates the resulting value" $ do
      let calculator = Bits.evaluate . Bits.decode . Bits.parseInput
      calculator "C200B40A82" `shouldBe` 3
      calculator "04005AC33890" `shouldBe` 54
      calculator "880086C3E88112" `shouldBe` 7
      calculator "CE00C43D881120" `shouldBe` 9
      calculator "D8005AC2A8F0" `shouldBe` 1
      calculator "F600BC2D8F" `shouldBe` 0
      calculator "9C005AC2F8F0" `shouldBe` 0
      calculator "9C0141080250320F1802104A08" `shouldBe` 1

  describe "parse input" $ do
    it "converts the transmission to binary" $ do
      let (_0, _1) = (Bits.Zero, Bits.One)
      let transmission = Bits.parseInput "D2FE28"
      transmission `shouldBe` [_1, _1, _0, _1, _0, _0, _1, _0, _1, _1, _1, _1, _1, _1, _1, _0, _0, _0, _1, _0, _1, _0, _0, _0]

  describe "decoding packets" $ do
    it "decodes a literal packet" $ do
      (Bits.decode . Bits.parseInput $ "D2FE28") `shouldBe` Bits.Literal 6 2021

    it "decodes an operator packet of type 0" $ do
      (Bits.decode . Bits.parseInput $ "38006F45291200")
        `shouldBe` Bits.Operator 1 Bits.Less [Bits.Literal 6 10, Bits.Literal 2 20]

    it "decodes an operator packet of type 1" $ do
      (Bits.decode . Bits.parseInput $ "EE00D40C823060")
        `shouldBe` Bits.Operator 7 Bits.Max [Bits.Literal 2 1, Bits.Literal 4 2, Bits.Literal 1 3]

  describe "value of binary number" $ do
    it "converts binary to number" $ do
      Bits.value [Bits.One, Bits.Zero, Bits.One, Bits.One] `shouldBe` 11
      Bits.value [Bits.One, Bits.One, Bits.One, Bits.Zero, Bits.Zero] `shouldBe` 28
