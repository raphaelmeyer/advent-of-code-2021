module Day20.TrenchMap where

import Data.Functor ((<&>))
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Day20.Grid

type Algorithm = Vector.Vector Pixel

data Pixel = Light | Dark deriving (Eq, Show)

type Image = [[Pixel]]

type View = Grid Pixel

readInput :: String -> IO [Text.Text]
readInput filename = readFile filename <&> Text.lines . Text.pack

parseInput :: [Text.Text] -> (Algorithm, Image)
parseInput input = (parseAlgo algo, parseImage image)
  where
    algo = head input
    image = dropWhile (== Text.empty) . tail $ input

parseAlgo :: Text.Text -> Algorithm
parseAlgo = Text.foldr (Vector.cons . pixel) Vector.empty
  where
    pixel '#' = Light
    pixel '.' = Dark
    pixel _ = undefined

parseImage :: [Text.Text] -> Image
parseImage = map parseRow

parseRow :: Text.Text -> [Pixel]
parseRow = Text.foldr parseColumn []

parseColumn :: Char -> [Pixel] -> [Pixel]
parseColumn '#' row = Light : row
parseColumn '.' row = Dark : row
parseColumn _ _ = undefined

enhance :: Image -> Algorithm -> Image
enhance image algo = enhance' (enhance' image algo Dark) algo outside'
  where
    outside' = algo Vector.! 0

enhance' :: Image -> Algorithm -> Pixel -> Image
enhance' image algo outside = enhanceRow . gridFromList . border outside $ image
  where
    enhanceRow GridEnd = []
    enhanceRow grid = row : enhanceRow grid'
      where
        (row, grid') = rowFold calc [] grid
        calc n row' = row' ++ [algo Vector.! binary outside n]

countLightPixels :: Image -> Int
countLightPixels = sum . map (foldl count 0)
  where
    count c Light = c + 1
    count c _ = c

binary :: Pixel -> [Maybe Pixel] -> Int
binary outside = foldl add' 0
  where
    add' v (Just Light) = 2 * v + 1
    add' v (Just Dark) = 2 * v
    add' v _ = 2 * v + if outside == Light then 1 else 0

border :: Pixel -> Image -> Image
border outside image = top : map border' image ++ [bottom]
  where
    top = replicate ((length . head $ image) + 2) outside
    bottom = replicate ((length . last $ image) + 2) outside
    border' row = outside : row ++ [outside]
