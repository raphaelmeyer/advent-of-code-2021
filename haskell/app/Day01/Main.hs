{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Day01.Sonar as Sonar

main :: IO ()
main = do
  input <- Sonar.parseInput "data/day-01.txt"
  let increase = Sonar.countIncrease input

  let windowSums = Sonar.windowSums input
  let incWindows = Sonar.countIncrease windowSums

  putStrLn "# Day 01 #"
  putStrLn $ "Part  I : " ++ show increase
  putStrLn $ "Part II : " ++ show incWindows
