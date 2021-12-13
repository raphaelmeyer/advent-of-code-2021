module Main where

import qualified Day12.Passage as Passage

main :: IO ()
main = do
  input <- Passage.readInput "data/day-12.txt"

  let graph = Passage.parseInput input

  let paths = Passage.searchPaths graph
  let numPaths = Passage.countPaths paths

  putStrLn "# Day 12 #"
  putStrLn $ "Part  I : " ++ show numPaths
  putStrLn $ "Part II : " ++ show (0 :: Int)
