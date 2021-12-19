{-# LANGUAGE OverloadedStrings #-}

module Day01 where

import qualified Day01.Sonar as Sonar

run :: IO ()
run = do
  input <- Sonar.parseInput "data/day-01.txt"
  let increase = Sonar.countIncrease input

  let windowSums = Sonar.windowSums input
  let incWindows = Sonar.countIncrease windowSums

  putStrLn ""
  putStrLn "# Day 01 #"
  putStrLn ""
  putStrLn $ "Part  I : " ++ show increase
  putStrLn $ "Part II : " ++ show incWindows
