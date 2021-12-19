module Day18 where

import qualified Day18.Snailfish as Snailfish

run :: IO ()
run = do
  input <- Snailfish.readInput "data/day-18.txt"

  let homework = Snailfish.parseInput input

  let result = Snailfish.sum homework
  let magnitude = Snailfish.magnitude result

  let maxMagnitude = Snailfish.maxMagnitude homework

  putStrLn ""
  putStrLn "# Day 18 #"
  putStrLn ""
  putStrLn $ "Part  I : " ++ show magnitude
  putStrLn $ "Part II : " ++ show maxMagnitude
