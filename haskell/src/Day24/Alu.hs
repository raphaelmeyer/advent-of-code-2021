{-# LANGUAGE OverloadedStrings #-}

module Day24.Alu where

import Data.Functor ((<&>))
import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.Text as Text

data Parameters = Parameters {getDivZ :: Bool, getAddX :: Int, getAddY :: Int} deriving (Eq, Show)

readInput :: String -> IO [Text.Text]
readInput filename = readFile filename <&> Text.lines . Text.pack

parseInput :: [Text.Text] -> [Parameters]
parseInput = foldr extractParameters [] . tail . Split.splitOn ["inp w"]
  where
    extractParameters [] _ = undefined
    extractParameters instructions params = Parameters {getDivZ = divZ, getAddX = addX, getAddY = addY} : params
      where
        divZ = instructions !! 3 == "div z 26"
        addX = readValue 4
        addY = readValue 14
        readValue index = read . last . Split.splitOn " " . Text.unpack $ instructions !! index

maxSerial :: [Parameters] -> Text.Text
maxSerial = findSerial maximize

minSerial :: [Parameters] -> Text.Text
minSerial = findSerial minimize

findSerial :: ([(Int, Int)] -> (Int, (Int, Int)) -> [(Int, Int)]) -> [Parameters] -> Text.Text
findSerial constraint parameters = Text.pack . concatMap (show . snd) . optimize $ pairs
  where
    optimize = List.sortBy sortByIndex . foldl constraint []
    pairs = match parameters
    sortByIndex (a, _) (b, _) = compare a b

match :: [Parameters] -> [(Int, (Int, Int))]
match parameters = pairs
  where
    (_, _, pairs) = foldl match' (0, [], []) parameters
    match' :: (Int, [(Int, Int)], [(Int, (Int, Int))]) -> Parameters -> (Int, [(Int, Int)], [(Int, (Int, Int))])
    match' (n, ys, ds) Parameters {getDivZ = False, getAddY = addY} = (n + 1, (addY, n) : ys, ds)
    match' (n, (y, ny) : ys, ds) Parameters {getDivZ = True, getAddX = addX} = (n + 1, ys, (addX + y, (ny, n)) : ds)
    match' _ _ = undefined

minimize :: [(Int, Int)] -> (Int, (Int, Int)) -> [(Int, Int)]
minimize ws (d, (a, b))
  | d >= 0 = (a, 1) : (b, 1 + d) : ws
  | otherwise = (a, 1 - d) : (b, 1) : ws

maximize :: [(Int, Int)] -> (Int, (Int, Int)) -> [(Int, Int)]
maximize ws (d, (a, b))
  | d < 0 = (a, 9) : (b, 9 + d) : ws
  | otherwise = (a, 9 - d) : (b, 9) : ws

verify :: [Parameters] -> [Int] -> Bool
verify parameters serial = check parameters serial 0 == 0
  where
    check [] [] z = z
    check (Parameters {getDivZ = divZ, getAddX = addX, getAddY = addY} : ps) (w : ws) z = check ps ws z'
      where
        z' = if neq then (z1 * 26) + (w + addY) else z1
        z1 = if divZ then div z 26 else z
        neq = (mod z 26 + addX) /= w
    check _ _ _ = undefined
