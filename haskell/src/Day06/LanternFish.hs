{-# LANGUAGE OverloadedStrings #-}

module Day06.LanternFish where

import qualified Data.Either as Either (rights)
import Data.Functor ((<&>))
import qualified Data.Text as Text
import qualified Data.Text.Read as Read (decimal)

readInput :: String -> IO Text.Text
readInput filename = readFile filename <&> Text.pack

parseInput :: Text.Text -> [Integer]
parseInput input = map (count numbers) (enumFromTo 0 8)
  where
    numbers = map fst . Either.rights . map Read.decimal $ Text.splitOn "," input
    count :: [Int] -> Int -> Integer
    count xs n = toInteger . length . filter (== n) $ xs

simulate :: Int -> Text.Text -> Integer
simulate days input = simulate' days (parseInput input)

simulate' :: Int -> [Integer] -> Integer
simulate' 0 fishes = sum fishes
simulate' days fishes = simulate' (days - 1) (fs ++ (f + r) : rs ++ [f])
  where
    (f : fs, r : rs) = splitAt 7 fishes
