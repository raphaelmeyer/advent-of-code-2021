module Main where

import qualified Day18.Snailfish as Snailfish

main :: IO ()
main = do
  input <- Snailfish.readInput "data/day-18.txt"

  let homework = Snailfish.parseInput input
  let result = Snailfish.sum homework
  let magnitude = Snailfish.magnitude result

  putStrLn "# Day 18 #"
  putStrLn $ "Part  I : " ++ show magnitude
  putStrLn $ "Part II : " ++ show (0 :: Int)
