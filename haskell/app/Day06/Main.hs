module Main where

import qualified Day06.LanternFish as Fish

main :: IO ()
main = do
  input <- Fish.readInput "data/day-06.txt"

  let population = Fish.simulate 80 input
  let bigPopulation = Fish.simulate 256 input

  putStrLn "# Day 06 #"
  putStrLn $ "Part  I : " ++ show population
  putStrLn $ "Part II : " ++ show bigPopulation
