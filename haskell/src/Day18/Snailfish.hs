module Day18.Snailfish where

import Control.Applicative (Alternative ((<|>)))
import Data.Functor ((<&>))
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Day18.Number as S
import qualified Day18.Parser as Parser
import Prelude hiding (sum)

readInput :: String -> IO [Text.Text]
readInput filename = readFile filename <&> Text.lines . Text.pack

parseInput :: [Text.Text] -> [S.Number]
parseInput = map Parser.parseNumber

sum :: [S.Number] -> S.Number
sum = foldl1 add

magnitude :: S.Number -> Int
magnitude (S.Regular n) = n
magnitude (S.Pair left right) = 3 * magnitude left + 2 * magnitude right

maxMagnitude :: [S.Number] -> Int
maxMagnitude numbers = maximum . map magnitude . concatMap sums $ pairs
  where
    pairs = [(x, y) | (x : ys) <- List.tails numbers, y <- ys]
    sums (a, b) = [sum [a, b], sum [b, a]]

-- snailfish math

reduce :: S.Number -> S.Number
reduce number = Maybe.fromMaybe number ((explode number <|> split number) >>= Just . reduce)

explode :: S.Number -> Maybe S.Number
explode number = exploding (number, []) 0 >>= Just . explode'

split :: S.Number -> Maybe S.Number
split number = splitting (number, []) >>= Just . split'

add :: S.Number -> S.Number -> S.Number
add a b = reduce (S.Pair a b)

-- explode

exploding :: (S.Number, Traversal) -> Int -> Maybe (S.Number, Traversal)
exploding (S.Regular _, _) _ = Nothing
exploding traversal depth
  | depth < 4 = exploding (left' traversal) (depth + 1) <|> exploding (right' traversal) (depth + 1)
  | otherwise = Just traversal

explode' :: (S.Number, Traversal) -> S.Number
explode' (S.Pair (S.Regular l) (S.Regular r), origin) = back' . addLeft l . addRight r $ (S.Regular 0, origin)
explode' _ = undefined

addLeft :: Int -> (S.Number, Traversal) -> (S.Number, Traversal)
addLeft _ (number, [FromLeft left]) = (number, [FromLeft left])
addLeft value (number, FromLeft left : ts) = left' . addLeft value . up' $ (number, FromLeft left : ts)
addLeft value (number, FromRight right : ts) = right' . up' . addLeft' value . left' . up' $ (number, FromRight right : ts)
addLeft _ (_, []) = undefined

addLeft' :: Int -> (S.Number, Traversal) -> (S.Number, Traversal)
addLeft' value (S.Regular n, origin) = (S.Regular (n + value), origin)
addLeft' value traversal = up' . addLeft' value . right' $ traversal

addRight :: Int -> (S.Number, Traversal) -> (S.Number, Traversal)
addRight _ (number, [FromRight right]) = (number, [FromRight right])
addRight value (number, FromRight right : ts) = right' . addRight value . up' $ (number, FromRight right : ts)
addRight value (number, FromLeft left : ts) = left' . up' . addRight' value . right' . up' $ (number, FromLeft left : ts)
addRight _ (_, []) = undefined

addRight' :: Int -> (S.Number, Traversal) -> (S.Number, Traversal)
addRight' value (S.Regular n, origin) = (S.Regular (n + value), origin)
addRight' value traversal = up' . addRight' value . left' $ traversal

-- split

splitting :: (S.Number, Traversal) -> Maybe (S.Number, Traversal)
splitting (S.Regular n, origin)
  | n >= 10 = Just (S.Regular n, origin)
  | otherwise = Nothing
splitting traversal = splitting (left' traversal) <|> splitting (right' traversal)

split' :: (S.Number, Traversal) -> S.Number
split' (S.Regular n, origin) = back' (S.Pair (S.Regular half) (S.Regular halfUp), origin)
  where
    half = div n 2
    halfUp = half + if odd n then 1 else 0
split' _ = undefined

-- move around within a number

data Origin = FromLeft S.Number | FromRight S.Number deriving (Eq, Show)

type Traversal = [Origin]

left' :: (S.Number, Traversal) -> (S.Number, Traversal)
left' (S.Pair left right, ts) = (left, FromLeft right : ts)
left' (S.Regular _, _) = undefined

right' :: (S.Number, Traversal) -> (S.Number, Traversal)
right' (S.Pair left right, ts) = (right, FromRight left : ts)
right' (S.Regular _, _) = undefined

up' :: (S.Number, Traversal) -> (S.Number, Traversal)
up' (left, FromLeft right : ts) = (S.Pair left right, ts)
up' (right, FromRight left : ts) = (S.Pair left right, ts)
up' _ = undefined

back' :: (S.Number, Traversal) -> S.Number
back' (number, []) = number
back' t = back' . up' $ t
