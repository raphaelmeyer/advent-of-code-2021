module Main where

import qualified Day10.Navi as Navi

main :: IO ()
main = do
  input <- Navi.readInput "data/day-10.txt"

  let syntax = Navi.parseInput input

  let syntaxErrorScore = Navi.syntaxErrorScore syntax
  let autoCompleteScore = Navi.autoCompleteScore syntax

  putStrLn "# Day 10 #"
  putStrLn $ "Part  I : " ++ show syntaxErrorScore
  putStrLn $ "Part II : " ++ show autoCompleteScore
