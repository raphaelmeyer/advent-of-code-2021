module Main where

import qualified Day12.Passage as Passage

main :: IO ()
main = do
  input <- Passage.readInput "data/day-12.txt"

  let graph = Passage.parseInput input

  let paths = Passage.countPaths graph
  let morePaths = Passage.countMorePaths graph

  putStrLn "# Day 12 #"
  putStrLn $ "Part  I : " ++ show paths
  putStrLn $ "Part II : " ++ show morePaths