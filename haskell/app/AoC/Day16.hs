module Day16 where

import qualified Day16.Bits as Bits

run :: IO ()
run = do
  input <- Bits.readInput "data/day-16.txt"

  let transmission = Bits.parseInput input

  let packet = Bits.decode transmission
  let versionSum = Bits.versionSum packet

  let result = Bits.evaluate packet

  putStrLn ""
  putStrLn "# Day 16 #"
  putStrLn ""
  putStrLn $ "Part  I : " ++ show versionSum
  putStrLn $ "Part II : " ++ show result
