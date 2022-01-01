module Day25 where

import qualified Day25.SeaCucumber as SeaCucumber

run :: IO ()
run = do
  input <- SeaCucumber.readInput "data/day-25.txt"

  let cucumbers = SeaCucumber.parseInput input
  let steps = SeaCucumber.stepsUntilStuck cucumbers

  putStrLn ""
  putStrLn "# Day 25 #"
  putStrLn ""
  putStrLn $ "Part  I : " ++ show steps
