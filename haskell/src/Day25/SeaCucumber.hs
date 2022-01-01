module Day25.SeaCucumber where

import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

data Cucumber = East | South deriving (Eq, Show)

type Grid = Map.Map (Int, Int) Cucumber

data Cucumbers = Cucumbers {grid :: Grid, bounds :: (Int, Int)} deriving (Eq, Show)

readInput :: String -> IO [Text.Text]
readInput filename = readFile filename <&> Text.lines . Text.pack

parseInput :: [Text.Text] -> Cucumbers
parseInput input = Cucumbers {grid = snd . foldl parseRow (0, Map.empty) $ input, bounds = (width, height)}
  where
    width = Text.length . head $ input
    height = length input
    parseRow (y, cucumbers) row = (y + 1, snd . Text.foldl parseColumn ((0, y), cucumbers) $ row)
    parseColumn ((x, y), cucumbers) c = ((x + 1, y), insert' c)
      where
        insert' '>' = Map.insert (x, y) East cucumbers
        insert' 'v' = Map.insert (x, y) South cucumbers
        insert' _ = cucumbers

stepsUntilStuck :: Cucumbers -> Int
stepsUntilStuck cucumbers
  | moved == cucumbers = 1
  | otherwise = 1 + stepsUntilStuck moved
  where
    moved = moveSouth . moveEast $ cucumbers

moveEast :: Cucumbers -> Cucumbers
moveEast cucumbers@Cucumbers {grid = before, bounds = (w, _)} = cucumbers {grid = Map.foldlWithKey tryMove Map.empty before}
  where
    tryMove after (x, y) South = Map.insert (x, y) South after
    tryMove after (x, y) East = case Map.lookup (x', y) before of
      Nothing -> Map.insert (x', y) East after
      _ -> Map.insert (x, y) East after
      where
        x' = mod (x + 1) w

moveSouth :: Cucumbers -> Cucumbers
moveSouth cucumbers@Cucumbers {grid = before, bounds = (_, h)} = cucumbers {grid = Map.foldlWithKey tryMove Map.empty before}
  where
    tryMove after (x, y) East = Map.insert (x, y) East after
    tryMove after (x, y) South = case Map.lookup (x, y') before of
      Nothing -> Map.insert (x, y') South after
      _ -> Map.insert (x, y) South after
      where
        y' = mod (y + 1) h
