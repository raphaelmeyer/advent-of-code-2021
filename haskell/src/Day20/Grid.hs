module Day20.Grid where

import qualified Data.Maybe as Maybe

data Row a = Row {getPrevious :: Maybe a, getPixel :: a, getNext :: [a]} deriving (Eq, Show)

data Grid a = Grid {getTop :: Maybe (Row a), getRow :: Row a, getBottom :: Maybe (Row a), getRows :: [[a]]} | GridEnd deriving (Eq, Show)

gridFromList :: [[a]] -> Grid a
gridFromList (row : rows) = Grid {getTop = Nothing, getRow = rowFromList row, getBottom = rowFromList <$> Maybe.listToMaybe rows, getRows = row : rows}
gridFromList _ = undefined

rowFromList :: [a] -> Row a
rowFromList (x : xs) = Row {getPrevious = Nothing, getPixel = x, getNext = xs}
rowFromList [] = undefined

advance :: Grid a -> Grid a
advance Grid {getRow = Row {getNext = []}, getRows = rows} = advanceRow rows
advance Grid {getTop = top, getRow = row, getBottom = bottom, getRows = rows} =
  Grid {getTop = advancePixel <$> top, getRow = advancePixel row, getBottom = advancePixel <$> bottom, getRows = rows}
advance _ = GridEnd

advanceRow :: [[a]] -> Grid a
advanceRow [_] = GridEnd
advanceRow (row : rows) = Grid {getTop = (Just . rowFromList) row, getRow = (rowFromList . head) rows, getBottom = rowFromList <$> (Maybe.listToMaybe . tail $ rows), getRows = rows}
advanceRow _ = undefined

advancePixel :: Row a -> Row a
advancePixel Row {getPixel = pixel, getNext = (x : xs)} = Row {getPrevious = Just pixel, getPixel = x, getNext = xs}
advancePixel _ = undefined

gridFold :: ([Maybe a] -> b -> b) -> b -> Grid a -> b
gridFold _ acc GridEnd = acc
gridFold f acc grid = gridFold f (f (neighbors grid) acc) (advance grid)

rowFold :: ([Maybe a] -> b -> b) -> b -> Grid a -> (b, Grid a)
rowFold f acc grid = case row of
  [] -> (acc', advanceRow rows)
  _ -> rowFold f acc' (advance grid)
  where
    acc' = f (neighbors grid) acc
    Grid {getRow = Row {getNext = row}, getRows = rows} = grid

neighbors :: Grid a -> [Maybe a]
neighbors grid = concatMap (\f -> neighbors' . f $ grid) [getTop, Just . getRow, getBottom]
  where
    neighbors' Nothing = [Nothing, Nothing, Nothing]
    neighbors' (Just Row {getPrevious = previous, getPixel = pixel, getNext = []}) = [previous, Just pixel, Nothing]
    neighbors' (Just Row {getPrevious = previous, getPixel = pixel, getNext = (n : _)}) = [previous, Just pixel, Just n]
