module Main where

import qualified Day05.Vents as Vents

main :: IO ()
main = do
  input <- Vents.readInput "data/day-05.txt"
  let dangerSimple = Vents.dangerSimple input
  let danger = Vents.danger input

  putStrLn "# Day 05 #"
  putStrLn $ "Part  I : " ++ show dangerSimple
  putStrLn $ "Part II : " ++ show danger
