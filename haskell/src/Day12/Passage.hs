{-# LANGUAGE OverloadedStrings #-}

module Day12.Passage where

import qualified Data.Char as Char
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text

data Node = Big Text.Text | Small Text.Text deriving (Eq, Show, Ord)

type Graph = Map.Map Node (Set.Set Node)

data Path = Path deriving (Eq, Show)

readInput :: String -> IO [Text.Text]
readInput filename = readFile filename <&> Text.lines . Text.pack

parseInput :: [Text.Text] -> Graph
parseInput input = foldr insertVertice Map.empty vertices
  where
    vertices = Maybe.mapMaybe (toTuple . map toNode . Text.splitOn "-") input
    toTuple [a, b] = Just (a, b)
    toTuple _ = Nothing
    toNode str
      | Text.all Char.isUpper str = Big str
      | otherwise = Small str

insertVertice :: (Node, Node) -> Graph -> Graph
insertVertice (a, b) = insert' a b . insert' b a
  where
    insert' from to graph = Map.alter (alter' to) from graph
    alter' to Nothing = Just (Set.singleton to)
    alter' to (Just nodes) = Just (Set.insert to nodes)

searchPaths :: Graph -> [Path]
searchPaths = searchPaths' []

searchPaths' :: [Node] -> Graph -> [Path]
searchPaths' [] graph = searchPaths' [Small "start"] graph
searchPaths' (Small "end" : _) _ = [Path]
searchPaths' (node : nodes) graph = Set.foldr visit [] (graph Map.! node)
  where
    visit (Small name) paths
      | Small name `notElem` nodes = searchPaths' (Small name : node : nodes) graph ++ paths
      | otherwise = paths
    visit node' paths = searchPaths' (node' : node : nodes) graph ++ paths

countPaths :: [Path] -> Int
countPaths = length