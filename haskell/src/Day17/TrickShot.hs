module Day17.TrickShot where

import Control.Applicative (Alternative (many, some), (<|>))
import qualified Data.Char as Char
import Data.Functor ((<&>))
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Util.Parser as Parser

type Position = (Int, Int)

type Velocity = (Int, Int)

type Range = (Int, Int)

data Area = Area Range Range deriving (Eq, Show)

data Trajectory = Trajectory {getMaxY :: Int, getHits :: Int} deriving (Eq, Show)

type InParser a = Parser.Parser String a

readInput :: String -> IO Text.Text
readInput filename = readFile filename <&> Text.pack

parseInput :: Text.Text -> Area
parseInput = fst . Maybe.fromJust . Parser.parse target . Text.unpack

calculateTrajectories :: Area -> Trajectory
calculateTrajectories targetArea
  | x1 > x2 = undefined
  | y1 > y2 = undefined
  | x1 < 0 = undefined
  | otherwise = Trajectory {getMaxY = maximum hits, getHits = length hits}
  where
    Area (x1, x2) (y1, y2) = targetArea
    hits = Maybe.catMaybes $ [shoot (0, 0) (vx, vy) targetArea 0 | vx <- [1 .. x2], vy <- [y1 .. abs y1]]

shoot :: Position -> Velocity -> Area -> Int -> Maybe Int
shoot (x, y) (vx, vy) t maxy
  | hit (nx, ny) t = Just nmaxy
  | onTrack (nx, ny) t = shoot (nx, ny) (nvx, vy - 1) t nmaxy
  | otherwise = Nothing
  where
    nx = x + vx
    ny = y + vy
    nvx
      | vx > 0 = vx - 1
      | otherwise = 0
    nmaxy = max ny maxy

hit :: Position -> Area -> Bool
hit (x, y) (Area (x1, x2) (y1, y2)) = x1 <= x && x <= x2 && y1 <= y && y <= y2

onTrack :: Position -> Area -> Bool
onTrack (x, y) (Area (_, x2) (y1, _)) = x <= x2 && y >= y1

-- parse input

target :: InParser Area
target = Area <$ string' "target area:" <* string' "x=" <*> range <* char ',' <* string' "y=" <*> range

range :: InParser (Int, Int)
range = (,) <$> int <* string ".." <*> int

string :: String -> InParser String
string [] = pure []
string (x : xs) = (:) <$> char x <*> string xs

string' :: String -> InParser String
string' = token . string

char :: Char -> InParser Char
char ch = satisfy (== ch)

int :: InParser Int
int = (negate <$ char '-' <*> nat) <|> nat

nat :: InParser Int
nat = read <$> some digit

digit :: InParser Char
digit = satisfy Char.isDigit

space :: InParser ()
space = () <$ many (satisfy Char.isSpace)

token :: InParser a -> InParser a
token p = space *> p <* space

satisfy :: (Char -> Bool) -> InParser Char
satisfy p = Parser.P satisfy'
  where
    satisfy' [] = Nothing
    satisfy' (b : bs) = if p b then Just (b, bs) else Nothing
