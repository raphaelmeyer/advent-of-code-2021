module Day22 where

import qualified Day22.Reactor as Reactor

run :: IO ()
run = do
  input <- Reactor.readInput "data/day-22.txt"

  let steps = Reactor.parseInput input

  let onCore = Reactor.preInitialize steps
  let onReactor = Reactor.initialize steps

  putStrLn ""
  putStrLn "# Day 22 #"
  putStrLn ""
  putStrLn $ "Part  I : " ++ show onCore
  putStrLn $ "Part II : " ++ show onReactor
