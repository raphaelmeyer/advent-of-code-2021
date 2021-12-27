{-# LANGUAGE OverloadedStrings #-}

module Day22.Reactor where

import Control.Applicative (Alternative ((<|>)))
import Data.Functor ((<&>))
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char as C (space, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Range = (Int, Int)

data Cuboid = Cuboid Range Range Range deriving (Eq, Show)

type Cuboids = [Cuboid]

data Step = On Cuboid | Off Cuboid deriving (Eq, Show)

type Steps = [Step]

type Sections = Map.Map (Range, Range) [Range]

readInput :: String -> IO [Text.Text]
readInput filename = readFile filename <&> Text.lines . Text.pack

parseInput :: [Text.Text] -> Steps
parseInput = map parseLine

initialize :: Steps -> Integer
initialize = snd . foldr execute ([], 0)

preInitialize :: Steps -> Integer
preInitialize = initialize . filter core
  where
    core (On cuboid) = core' cuboid
    core (Off cuboid) = core' cuboid
    core' (Cuboid (x1, x2) (y1, y2) (z1, z2)) = all inside' [x1, x2, y1, y2, z1, z2]
    inside' a = -50 <= a && a <= 51

execute :: Step -> (Cuboids, Integer) -> (Cuboids, Integer)
execute (Off cuboid) (cuboids, volume) = (cuboid : cuboids, volume)
execute step@(On cuboid) (cuboids, volume) = (cuboid : cuboids, volume + additionalVolume step cuboids)

additionalVolume :: Step -> Cuboids -> Integer
additionalVolume Off {} _ = 0
additionalVolume (On cuboid) [] = cuboidVolume cuboid
additionalVolume (On cuboid) cuboids = cuboidVolume cuboid - unionVolume intersections
  where
    intersections = Maybe.mapMaybe (intersection cuboid) cuboids

intersection :: Cuboid -> Cuboid -> Maybe Cuboid
intersection (Cuboid xa ya za) (Cuboid xb yb zb) = Cuboid <$> intersect' xa xb <*> intersect' ya yb <*> intersect' za zb
  where
    intersect' (a1, a2) (b1, b2)
      | a < b = Just (a, b)
      | otherwise = Nothing
      where
        a = max a1 b1
        b = min a2 b2

cuboidVolume :: Cuboid -> Integer
cuboidVolume (Cuboid x y z) = product . map (toInteger . uncurry (flip (-))) $ [x, y, z]

unionVolume :: Cuboids -> Integer
unionVolume [] = 0
unionVolume cuboids = sectionsVolume grouped
  where
    grouped = foldl alter' Map.empty sections
    sections = [(x, y, z) | cuboid@(Cuboid _ _ z) <- cuboids, x <- xSegments cuboid, y <- ySegments cuboid]
    xCorners = List.sort . List.nub . concatMap (\(Cuboid (x1, x2) _ _) -> [x1, x2]) $ cuboids
    yCorners = List.sort . List.nub . concatMap (\(Cuboid _ (y1, y2) _) -> [y1, y2]) $ cuboids
    xSegments (Cuboid x _ _) = splitIntoSegments x xCorners
    ySegments (Cuboid _ y _) = splitIntoSegments y yCorners
    alter' m (x, y, z) = Map.alter add' (x, y) m
      where
        add' Nothing = Just [z]
        add' (Just zs) = Just (z : zs)
    sectionsVolume = Map.foldlWithKey sectionVolume 0
      where
        sectionVolume v (x, y) zs = v + foldl (\v' z -> v' + cuboidVolume (Cuboid x y z)) 0 (intervals zs)

splitIntoSegments :: Range -> [Int] -> [Range]
splitIntoSegments range [] = [range]
splitIntoSegments (from, to) points = foldl rangify [] $ filter inside points ++ [to]
  where
    inside x = from < x && x < to
    rangify [] value = [(from, value)]
    rangify ranges@((_, b) : _) value = (b, value) : ranges

intervals :: [Range] -> [Range]
intervals ranges = foldl unionify [] sorted
  where
    sorted = List.sort . List.nub $ ranges
    unionify [] range = [range]
    unionify unified@((a, b) : rs) (c, d)
      | b < c = (c, d) : unified
      | c <= b && b < d = (a, d) : rs
      | otherwise = unified

-- parse input

type Parser = MP.Parsec Void Text.Text

parseLine :: Text.Text -> Step
parseLine input = case MP.runParser grammar "" input of
  Left _ -> undefined
  Right step -> step

grammar :: Parser Step
grammar = (On <$ C.string "on" <|> Off <$ C.string "off") <*> parseCuboid

parseCuboid :: Parser Cuboid
parseCuboid =
  Cuboid <$ C.space <* C.string "x="
    <*> parseRange <* C.string ",y="
    <*> parseRange <* C.string ",z="
    <*> parseRange

parseRange :: Parser Range
parseRange = (,) <$> integer <* C.string ".." <*> ((+ 1) <$> integer)
  where
    integer = L.signed (pure ()) L.decimal
