module Day07 where

import qualified Day07.CrabSubmarine as Crab

run :: IO ()
run = do
  input <- Crab.readInput "data/day-07.txt"
  let positions = Crab.parseInput input

  let fuelConstant = Crab.leastFuelConstant positions
  let fuelIncreasing = Crab.leastFuelIncreasing positions

  putStrLn ""
  putStrLn "# Day 07 #"
  putStrLn ""
  putStrLn $ "Part  I : " ++ show fuelConstant
  putStrLn $ "Part II : " ++ show fuelIncreasing
