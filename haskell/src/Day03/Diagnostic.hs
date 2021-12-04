module Day03.Diagnostic where

import Data.Functor ((<&>))
import qualified Data.Text as Text

parseInput :: String -> IO [Text.Text]
parseInput filename = readFile filename <&> Text.lines . Text.pack

powerRate :: [Text.Text] -> Int
powerRate numbers = gamma * epsilon
  where
    (gamma, epsilon) = calcuateRates numbers

calcuateRates :: [Text.Text] -> (Int, Int)
calcuateRates numbers = mostCommonInt (sumUpBits (map toBitList numbers)) half
  where
    half = div (length numbers) 2

mostCommonInt :: [Int] -> Int -> (Int, Int)
mostCommonInt counts half = foldl addBit (0, 0) counts
  where
    addBit (gamma, epsilon) count
      | count > half = (2 * gamma + 1, 2 * epsilon)
      | otherwise = (2 * gamma, 2 * epsilon + 1)

sumUpBits :: [[Int]] -> [Int]
sumUpBits = foldl1 (zipWith (+))

toBitList :: Text.Text -> [Int]
toBitList = Text.foldr ((:) . bit) []
  where
    bit '1' = 1 :: Int
    bit _ = 0 :: Int

-- Part 2

lifeSupportRating :: [Text.Text] -> Int
lifeSupportRating numbers = oxygen * co2scrubs
  where
    tree = foldl insertBits Nil (map toBitList numbers)
    oxygen = countOxygen tree
    co2scrubs = countCO2Scrubs tree

data Tree = Nil | Node (Int, Int) Tree Tree deriving (Show, Eq)

insertBits :: Tree -> [Int] -> Tree
insertBits Nil (0 : xs) = Node (1, 0) (insertBits Nil xs) Nil
insertBits Nil (_ : xs) = Node (0, 1) Nil (insertBits Nil xs)
insertBits (Node (zeros, ones) zero one) (0 : xs) = Node (zeros + 1, ones) (insertBits zero xs) one
insertBits (Node (zeros, ones) zero one) (_ : xs) = Node (zeros, ones + 1) zero (insertBits one xs)
insertBits tree [] = tree

countOxygen :: Tree -> Int
countOxygen = countOxygen' 0

countOxygen' :: Int -> Tree -> Int
countOxygen' value Nil = value
countOxygen' value (Node (zeros, ones) zero one)
  | zeros <= ones = countOxygen' (2 * value + 1) one
  | otherwise = countOxygen' (2 * value) zero

countCO2Scrubs :: Tree -> Int
countCO2Scrubs = countCO2Scrubs' 0

countCO2Scrubs' :: Int -> Tree -> Int
countCO2Scrubs' value Nil = value
countCO2Scrubs' value (Node (zeros, ones) zero one)
  | useZero = countCO2Scrubs' (2 * value) zero
  | otherwise = countCO2Scrubs' (2 * value + 1) one
  where
    useZero = ones == 0 || (zeros /= 0 && zeros <= ones)
