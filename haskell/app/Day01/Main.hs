{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Day01.Sonar as Sonar

main :: IO ()
main = do
  input <- Sonar.parseInput "data/day01.txt"
  let increase = Sonar.countIncrease input

  putStrLn "# Day 02 #"
  putStrLn $ "Part I : " ++ show increase
  putStrLn $ "Part II : "
