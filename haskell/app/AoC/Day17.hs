module Day17 where

import qualified Day17.TrickShot as TrickShot

run :: IO ()
run = do
  input <- TrickShot.readInput "data/day-17.txt"

  let area = TrickShot.parseInput input
  let trajectory = TrickShot.calculateTrajectories area

  putStrLn ""
  putStrLn "# Day 17 #"
  putStrLn ""
  putStrLn $ "Part  I : " ++ show (TrickShot.getMaxY trajectory)
  putStrLn $ "Part II : " ++ show (TrickShot.getHits trajectory)
