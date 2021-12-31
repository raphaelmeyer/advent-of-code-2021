module Main where

import Control.Applicative ((<**>))
import qualified Control.Applicative as Applicative
import qualified Data.Map as Map
import qualified Day01
import qualified Day03
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day09
import qualified Day10
import qualified Day12
import qualified Day13
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day20
import qualified Day22
import qualified Day24
import qualified Options.Applicative as Opt

data Options = Options {getDay :: Maybe Int} deriving (Show)

options :: Opt.Parser Options
options = Options <$> Applicative.optional (Opt.option Opt.auto $ Opt.long "day" <> Opt.help "run solution of a single day" <> Opt.metavar "DAY")

solutions :: Map.Map Int (IO ())
solutions =
  Map.fromList
    [ (1, Day01.run),
      (3, Day03.run),
      (5, Day05.run),
      (6, Day06.run),
      (7, Day07.run),
      (9, Day09.run),
      (10, Day10.run),
      (12, Day12.run),
      (13, Day13.run),
      (16, Day16.run),
      (17, Day17.run),
      (18, Day18.run),
      (20, Day20.run),
      (22, Day22.run),
      (24, Day24.run)
    ]

main :: IO ()
main = do
  opts <- Opt.execParser (Opt.info (options <**> Opt.helper) (Opt.fullDesc <> Opt.progDesc "run advent of code 2021 solutions" <> Opt.header "Advent of Code 2021"))
  runSolutions (getDay opts)

runSolutions :: Maybe Int -> IO ()
runSolutions Nothing = sequence_ . Map.elems $ solutions
runSolutions (Just day) = runSolution $ Map.lookup day solutions
  where
    runSolution (Just solution) = solution
    runSolution Nothing = putStrLn $ "No solution for day " ++ show day
