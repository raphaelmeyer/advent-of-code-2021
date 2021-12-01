module Day01.Sonar (countIncrease, parseInput) where

import Data.Text (Text)

parseInput :: Text -> IO [Int]
-- parseInput file = readFile file <&> Text.strip . Text.pack
parseInput _ = return [1, 2, 3]

countIncrease :: [Int] -> Int
countIncrease (a : b : rest) =
  if a < b
    then 1 + countIncrease (b : rest)
    else countIncrease (b : rest)
countIncrease _ = 0
