module Main where

import qualified Day09.LavaTubes as LavaTubes

main :: IO ()
main = do
  input <- LavaTubes.readInput "data/day-09.txt"
  let grid = LavaTubes.parseInput input

  let riskLevelSum = LavaTubes.riskLevelSum grid

  putStrLn "# Day 09 #"
  putStrLn $ "Part  I : " ++ show riskLevelSum
  putStrLn $ "Part II : " ++ show (0 :: Int)
