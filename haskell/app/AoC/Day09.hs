module Day09 where

import qualified Day09.LavaTubes as LavaTubes

run :: IO ()
run = do
  input <- LavaTubes.readInput "data/day-09.txt"
  let grid = LavaTubes.parseInput input

  let riskLevelSum = LavaTubes.riskLevelSum grid
  let basinSize = LavaTubes.basinSize grid

  putStrLn ""
  putStrLn "# Day 09 #"
  putStrLn ""
  putStrLn $ "Part  I : " ++ show riskLevelSum
  putStrLn $ "Part II : " ++ show basinSize
