module Main where

import qualified Day17.TrickShot as TrickShot

main :: IO ()
main = do
  input <- TrickShot.readInput "data/day-17.txt"

  let area = TrickShot.parseInput input
  let trajectory = TrickShot.calculateTrajectories area

  putStrLn "# Day 17 #"
  putStrLn $ "Part  I : " ++ show (TrickShot.getMaxY trajectory)
  putStrLn $ "Part II : " ++ show (TrickShot.getHits trajectory)
