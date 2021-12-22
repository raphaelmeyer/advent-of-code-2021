module Day20 where

import qualified Day20.TrenchMap as TrenchMap

run :: IO ()
run = do
  input <- TrenchMap.readInput "data/day-20.txt"

  let (algo, image) = TrenchMap.parseInput input

  let enhanced = TrenchMap.enhance image algo
  let veryEnhanced = iterate (`TrenchMap.enhance` algo) image !! 25

  putStrLn ""
  putStrLn "# Day 20 #"
  putStrLn ""
  putStrLn $ "Part  I : " ++ show (TrenchMap.countLightPixels enhanced)
  putStrLn $ "Part II : " ++ show (TrenchMap.countLightPixels veryEnhanced)
