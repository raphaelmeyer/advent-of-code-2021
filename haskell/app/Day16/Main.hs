module Main where

import qualified Day16.Bits as Bits

main :: IO ()
main = do
  input <- Bits.readInput "data/day-16.txt"

  let transmission = Bits.parseInput input

  let packet = Bits.decode transmission
  let versionSum = Bits.versionSum packet

  putStrLn "# Day 16 #"
  putStrLn $ "Part  I : " ++ show versionSum
  putStrLn $ "Part II : " ++ show (0 :: Int)
