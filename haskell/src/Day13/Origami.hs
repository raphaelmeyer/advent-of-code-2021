{-# LANGUAGE OverloadedStrings #-}

module Day13.Origami where

import qualified Data.Either as Either
import Data.Functor ((<&>))
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Read as Read (decimal)

type Point = (Int, Int)

type Paper = [Point]

data Fold = FoldX Int | FoldY Int deriving (Eq, Show)

type Instructions = [Fold]

readInput :: String -> IO [Text.Text]
readInput filename = readFile filename <&> Text.lines . Text.pack

parseInput :: [Text.Text] -> (Paper, Instructions)
parseInput = foldr parseLine ([], [])

parseLine :: Text.Text -> (Paper, Instructions) -> (Paper, Instructions)
parseLine line (paper, instructions) = (maybe id (:) point paper, maybe id (:) instruction instructions)
  where
    point = parsePoint line
    instruction = parseInstruction line

parsePoint :: Text.Text -> Maybe Point
parsePoint line = toPoint values
  where
    values = map fst . Either.rights . map Read.decimal $ Text.splitOn "," line
    toPoint [x, y] = Just (x, y)
    toPoint _ = Nothing

parseInstruction :: Text.Text -> Maybe Fold
parseInstruction line
  | Text.isPrefixOf "fold along x=" line = FoldX <$> value
  | Text.isPrefixOf "fold along y=" line = FoldY <$> value
  | otherwise = Nothing
  where
    value = fmap fst . Either.either (const Nothing) Just . Read.decimal . last $ Text.splitOn "=" line

fold :: Paper -> Fold -> Paper
fold paper (FoldX fx) = List.nub . map fold' $ paper
  where
    fold' (x, y)
      | x > fx = (2 * fx - x, y)
      | otherwise = (x, y)
fold paper (FoldY fy) = List.nub . map fold' $ paper
  where
    fold' (x, y)
      | y > fy = (x, 2 * fy - y)
      | otherwise = (x, y)

finish :: Paper -> Instructions -> Paper
finish = foldl fold

countDots :: Paper -> Int
countDots = length

showPaper :: Paper -> Text.Text
showPaper paper = Text.intercalate "\n" [line y | y <- [0 .. maxY]]
  where
    maxX = maximum . map fst $ paper
    maxY = maximum . map snd $ paper
    line y = Text.concat [if (x, y) `elem` paper then "x" else " " | x <- [0 .. maxX]]
