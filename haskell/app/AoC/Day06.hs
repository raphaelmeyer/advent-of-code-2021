module Day06 where

import qualified Day06.LanternFish as Fish

run :: IO ()
run = do
  input <- Fish.readInput "data/day-06.txt"

  let population = Fish.simulate 80 input
  let bigPopulation = Fish.simulate 256 input

  putStrLn ""
  putStrLn "# Day 06 #"
  putStrLn ""
  putStrLn $ "Part  I : " ++ show population
  putStrLn $ "Part II : " ++ show bigPopulation
