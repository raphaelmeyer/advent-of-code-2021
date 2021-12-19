module Day18.Number where

data Number = Pair Number Number | Regular Int deriving (Eq, Show)
