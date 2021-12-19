module Day03 where

import qualified Day03.Diagnostic as Diagnostic

run :: IO ()
run = do
  input <- Diagnostic.parseInput "data/day-03.txt"

  let powerRate = Diagnostic.powerRate input
  let lifeSupportRating = Diagnostic.lifeSupportRating input

  putStrLn ""
  putStrLn "# Day 03 #"
  putStrLn ""
  putStrLn $ "Part  I : " ++ show powerRate
  putStrLn $ "Part II : " ++ show lifeSupportRating
