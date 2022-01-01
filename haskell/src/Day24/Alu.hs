{-# LANGUAGE OverloadedStrings #-}

module Day24.Alu where

import Data.Functor ((<&>))
import qualified Data.List.Split as Split
import qualified Data.Text as Text

data Action = Push Int | Pop Int deriving (Eq, Show)

data Step = Step {getStack :: [Int], getSerial :: [[Int]]} deriving (Eq, Show)

type Optimize = Int -> (Int, Int)

readInput :: String -> IO [Text.Text]
readInput filename = readFile filename <&> Text.lines . Text.pack

parseInput :: [Text.Text] -> [Action]
parseInput = foldr extractActions [] . tail . Split.splitOn ["inp w"]
  where
    extractActions [] _ = undefined
    extractActions instructions actions
      | instructions !! 3 == "div z 26" = Pop addX : actions
      | otherwise = Push addY : actions
      where
        addX = readValue 4
        addY = readValue 14
        readValue index = read . last . Split.splitOn " " . Text.unpack $ instructions !! index

maxSerial :: [Action] -> Text.Text
maxSerial = Text.pack . concatMap show . concat . getSerial . foldl (apply maximize) empty

minSerial :: [Action] -> Text.Text
minSerial = Text.pack . concatMap show . concat . getSerial . foldl (apply minimize) empty

apply :: Optimize -> Step -> Action -> Step
apply _ Step {getStack = stack, getSerial = serial} (Push y) =
  Step {getStack = y : stack, getSerial = [] : serial}
apply optimize Step {getStack = y : stack, getSerial = s : s' : serial} (Pop x) =
  Step {getStack = stack, getSerial = (s' ++ [a] ++ s ++ [b]) : serial}
  where
    (a, b) = optimize (x + y)
apply _ _ _ = undefined

empty :: Step
empty = Step {getStack = [], getSerial = [[]]}

minimize :: Optimize
minimize d
  | d >= 0 = (1, 1 + d)
  | otherwise = (1 - d, 1)

maximize :: Optimize
maximize d
  | d < 0 = (9, 9 + d)
  | otherwise = (9 - d, 9)
