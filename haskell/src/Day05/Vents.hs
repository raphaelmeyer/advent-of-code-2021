{-# LANGUAGE OverloadedStrings #-}

module Day05.Vents where

import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (char, space, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = MP.Parsec Void Text.Text

data Position = Position Int Int deriving (Show, Eq, Ord)

data Line = Line {getFrom :: Position, getTo :: Position} deriving (Show, Eq)

type Diagram = Map.Map Position Int

readInput :: String -> IO [Text.Text]
readInput filename = readFile filename <&> Text.lines . Text.pack

parseInput :: [Text.Text] -> [Line]
parseInput = map parseLine

dangerSimple :: [Text.Text] -> Int
dangerSimple = length . findDanger . createDiagram . removeSlant . parseInput

danger :: [Text.Text] -> Int
danger = length . findDanger . createDiagram . parseInput

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

parseLine :: Text.Text -> Line
parseLine input = case MP.runParser grammar "" input of
  Left _ -> Line (Position 0 0) (Position 0 0)
  Right line -> line
  where
    pInt :: Parser Int
    pInt = lexeme L.decimal
    pArrow = lexeme (string "->")
    pPosition :: Parser Position
    pPosition = Position <$> pInt <* char ',' <*> pInt
    grammar :: Parser Line
    grammar = Line <$> pPosition <* pArrow <*> pPosition

removeSlant :: [Line] -> [Line]
removeSlant = filter slant
  where
    slant (Line (Position x1 y1) (Position x2 y2)) = x1 == x2 || y1 == y2

createDiagram :: [Line] -> Diagram
createDiagram vents = foldl addPos Map.empty (concatMap findPositions vents)
  where
    addPos diagram pos = Map.alter incCount pos diagram
    incCount Nothing = Just 1
    incCount (Just count) = Just (count + 1)

findPositions :: Line -> [Position]
findPositions (Line (Position x1 y1) (Position x2 y2))
  | x1 == x2 = Position x1 <$> fromTo y1 y2
  | y1 == y2 = (`Position` y1) <$> fromTo x1 x2
  | otherwise = zipWith Position (fromTo x1 x2) (fromTo y1 y2)
  where
    fromTo a b
      | a < b = enumFromTo a b
      | otherwise = reverse (enumFromTo b a)

findDanger :: Diagram -> [Position]
findDanger diagram = Map.keys (Map.filter (>= 2) diagram)
