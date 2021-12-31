{-# LANGUAGE OverloadedStrings #-}

module Day24 where

import qualified Data.Text.IO as Text
import qualified Day24.Alu as Alu

run :: IO ()
run = do
  input <- Alu.readInput "data/day-24.txt"
  let parameters = Alu.parseInput input

  let maxSerial = Alu.maxSerial parameters
  let minSerial = Alu.minSerial parameters

  putStrLn ""
  putStrLn "# Day 24 #"
  putStrLn ""

  putStr "Part  I : "
  Text.putStrLn maxSerial

  putStr "Part II : "
  Text.putStrLn minSerial
