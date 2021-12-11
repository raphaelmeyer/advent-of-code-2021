module Day09.LavaTubes where

import qualified Data.Char as Char (digitToInt)
import Data.Functor ((<&>))
import qualified Data.Text as Text
import qualified Day09.Grid as Grid

readInput :: String -> IO [Text.Text]
readInput filename = readFile filename <&> Text.lines . Text.pack

parseInput :: [Text.Text] -> [[Int]]
parseInput = map (map Char.digitToInt . Text.unpack)

riskLevelSum :: [[Int]] -> Int
riskLevelSum = sum . map (+ 1) . findLowPoints

findLowPoints :: [[Int]] -> [Int]
findLowPoints list = Grid.foldWithNeighbors isLowPoint [] grid
  where
    grid = Grid.fromList list
    isLowPoint acc (value, neighbors)
      | all (value <) neighbors = value : acc
      | otherwise = acc
