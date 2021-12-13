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
    insert' _ (Small "start") graph = graph
    insert' (Small "end") _ graph = graph
    insert' from to graph = Map.alter (alter' to) from graph
    alter' to Nothing = Just (Set.singleton to)
    alter' to (Just nodes) = Just (Set.insert to nodes)

countPaths :: Graph -> Int
countPaths = searchPaths [] False

countMorePaths :: Graph -> Int
countMorePaths = searchPaths [] True

searchPaths :: [Node] -> Bool -> Graph -> Int
searchPaths [] twice graph = searchPaths [Small "start"] twice graph
searchPaths (Small "end" : _) _ _ = 1
searchPaths (node : nodes) twice graph = Set.foldr visit 0 (graph Map.! node)
  where
    visit (Small name) paths
      | Small name `notElem` nodes = searchPaths (Small name : node : nodes) twice graph + paths
      | twice = searchPaths (Small name : node : nodes) False graph + paths
      | otherwise = paths
    visit node' paths = searchPaths (node' : node : nodes) twice graph + paths
