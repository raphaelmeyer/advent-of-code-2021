module Main where

import qualified Data.Text as Text
import qualified Day13.Origami as Origami

main :: IO ()
main = do
  input <- Origami.readInput "data/day-13.txt"

  let (paper, instructions) = Origami.parseInput input

  let dots = Origami.countDots . Origami.fold paper . head $ instructions

  let code = Origami.finish paper instructions

  putStrLn "# Day 13 #"
  putStrLn $ "Part  I : " ++ show dots
  putStrLn "Part II : "

  putStrLn . Text.unpack $ Origami.showPaper code
