{-# LANGUAGE OverloadedStrings #-}

module Day07.CrabSubmarine where

import qualified Data.Either as Either (rights)
import Data.Functor ((<&>))
import qualified Data.List as List (sort, splitAt)
import qualified Data.Text as Text
import qualified Data.Text.Read as Read (decimal)

readInput :: String -> IO Text.Text
readInput filename = readFile filename <&> Text.pack

parseInput :: Text.Text -> [Int]
parseInput input = map fst . Either.rights . map Read.decimal $ Text.splitOn "," input

leastFuelConstant :: [Int] -> Int
leastFuelConstant input = sum . map cost $ sorted
  where
    sorted = List.sort input
    (_, median : _) = List.splitAt (div (length sorted) 2) sorted
    cost = abs . (median -)

leastFuelIncreasing :: [Int] -> Int
leastFuelIncreasing input = minimum . map (`total` input) $ [-1, 0, 1]
  where
    almostMean = div (sum input) (length input)
    cost off x = div (deviation * (deviation + 1)) 2
      where
        deviation = abs (almostMean - x + off)
    total off = sum . map (cost off)
