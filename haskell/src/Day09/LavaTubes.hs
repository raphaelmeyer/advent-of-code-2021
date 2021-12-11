module Day09.LavaTubes where

import qualified Data.Char as Char (digitToInt)
import Data.Functor ((<&>))
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import qualified Data.Maybe as Maybe (mapMaybe)
import qualified Data.Text as Text
import qualified Day09.Grid as Grid

readInput :: String -> IO [Text.Text]
readInput filename = readFile filename <&> Text.lines . Text.pack

parseInput :: [Text.Text] -> [[Int]]
parseInput = map (map Char.digitToInt . Text.unpack)

riskLevelSum :: [[Int]] -> Int
riskLevelSum = sum . map (+ 1) . findLowPoints

basinSize :: [[Int]] -> Int
basinSize = product . take 3 . List.sortBy (flip compare) . findBasins

findLowPoints :: [[Int]] -> [Int]
findLowPoints list = Grid.foldWithNeighbors isLowPoint [] grid
  where
    grid = Grid.fromList list
    isLowPoint acc (value, neighbors)
      | lowest = value : acc
      | otherwise = acc
      where
        lowest = all (value <) $ Maybe.mapMaybe (\f -> f neighbors) [Grid.nbLeft, Grid.nbRight, Grid.nbUp, Grid.nbDown]

findBasins :: [[Int]] -> [Int]
findBasins list = map countLocations connected
  where
    labeled = firstPass list
    connections = secondPass labeled
    connected = foldr insertConnection [] connections
    flat = concat labeled
    countLocations ids = length . filter (`IntSet.member` ids) $ flat

firstPass :: [[Int]] -> [[Int]]
firstPass list = labeled
  where
    grid = Grid.fromList list
    (_, _, labeled) = Grid.foldWithNeighbors setLabel (1, [], []) grid

secondPass :: [[Int]] -> [(Int, Int)]
secondPass labeled = List.nub . Grid.foldWithNeighbors checkConnection [] $ grid
  where
    grid = Grid.fromList labeled

type LabelAcc = (Int, [Int], [[Int]])

setLabel :: LabelAcc -> (Int, Grid.Neighbors Int) -> LabelAcc
setLabel (label, row, acc) (9, Grid.Neighbors {Grid.nbLeft = Nothing}) = (label, [], (0 : row) : acc)
setLabel (label, row, acc) (_, Grid.Neighbors {Grid.nbLeft = Nothing}) = (label + 1, [], (label : row) : acc)
setLabel (label, row, acc) (9, _) = (label, 0 : row, acc)
setLabel (label, row, acc) (_, Grid.Neighbors {Grid.nbLeft = Just 9}) = (label + 1, label : row, acc)
setLabel (label, row, acc) (_, _) = (label, label : row, acc)

type ConnectionAcc = [(Int, Int)]

checkConnection :: ConnectionAcc -> (Int, Grid.Neighbors Int) -> ConnectionAcc
checkConnection acc (0, _) = acc
checkConnection acc (label, neighbors) = Maybe.mapMaybe addConnection [Grid.nbRight, Grid.nbDown] ++ acc
  where
    addConnection getNb = case getNb neighbors of
      Nothing -> Nothing
      Just 0 -> Nothing
      Just other -> if label == other then Nothing else Just (label, other)

insertConnection :: (Int, Int) -> [IntSet.IntSet] -> [IntSet.IntSet]
insertConnection (a, b) [] = [IntSet.fromList [a, b]]
insertConnection (a, b) connections = case (findA, findB) of
  (Nothing, Nothing) -> IntSet.fromList [a, b] : connections
  (Just ia, Nothing) -> insertInSet ia b
  (Nothing, Just ib) -> insertInSet ib a
  (Just ia, Just ib) -> if ia == ib then connections else mergeSets (if ia < ib then (ia, ib) else (ib, ia))
  where
    findA = List.findIndex (IntSet.member a) connections
    findB = List.findIndex (IntSet.member b) connections
    mergeSets (ia, ib) = IntSet.union setA setB : front ++ middle ++ back
      where
        (containsA, setB : back) = List.splitAt ib connections
        (front, setA : middle) = List.splitAt ia containsA
    insertInSet idx value = front ++ (IntSet.insert value set : back)
      where
        (front, set : back) = List.splitAt idx connections
