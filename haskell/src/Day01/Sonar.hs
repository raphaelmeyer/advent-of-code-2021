module Day01.Sonar (countIncrease, parseInput, windowSums) where

import qualified Data.Either as Either (rights)
import Data.Functor ((<&>))
import qualified Data.List as List (tails)
import qualified Data.Text as Text
import qualified Data.Text.Read as Read (decimal)

parseInput :: String -> IO [Int]
parseInput file = readFile file <&> parseNumbers . Text.lines . Text.pack

parseNumbers :: [Text.Text] -> [Int]
parseNumbers = map fst . Either.rights . map Read.decimal

countIncrease :: [Int] -> Int
countIncrease (a : b : rest) =
  if a < b
    then 1 + countIncrease (b : rest)
    else countIncrease (b : rest)
countIncrease _ = 0

windowSums :: [Int] -> [Int]
windowSums = map (sum . take 3) . filter ((>= 3) . length) . List.tails
