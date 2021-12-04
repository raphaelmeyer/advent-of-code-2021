module Main where

import qualified Day03.Diagnostic as Diagnostic

main :: IO ()
main = do
  input <- Diagnostic.parseInput "data/day-03.txt"

  let powerRate = Diagnostic.powerRate input
  let lifeSupportRating = Diagnostic.lifeSupportRating input

  putStrLn "# Day 03 #"
  putStrLn $ "Part  I : " ++ show powerRate
  putStrLn $ "Part II : " ++ show lifeSupportRating
