module Day05 where

import qualified Day05.Vents as Vents

run :: IO ()
run = do
  input <- Vents.readInput "data/day-05.txt"
  let dangerSimple = Vents.dangerSimple input
  let danger = Vents.danger input

  putStrLn ""
  putStrLn "# Day 05 #"
  putStrLn ""
  putStrLn $ "Part  I : " ++ show dangerSimple
  putStrLn $ "Part II : " ++ show danger
