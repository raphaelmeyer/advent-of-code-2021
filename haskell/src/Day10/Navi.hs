module Day10.Navi where

import Data.Functor ((<&>))
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text

data Syntax = Ok | Error Char | Incomplete [Char] deriving (Eq, Show)

data Parse = Parse [Char] | InvalidToken Char deriving (Eq, Show)

readInput :: String -> IO [Text.Text]
readInput filename = readFile filename <&> Text.lines . Text.pack

parseInput :: [Text.Text] -> [Syntax]
parseInput = map checkSyntax

syntaxErrorScore :: [Syntax] -> Int
syntaxErrorScore = foldl evaluate 0
  where
    evaluate score syntax = case syntax of
      Error ')' -> score + 3
      Error ']' -> score + 57
      Error '}' -> score + 1197
      Error '>' -> score + 25137
      _ -> score

autoCompleteScore :: [Syntax] -> Int
autoCompleteScore parsed = score
  where
    scores = List.sort . Maybe.mapMaybe scoreCompletion $ parsed
    (_, score : _) = List.splitAt (length scores `div` 2) scores

scoreCompletion :: Syntax -> Maybe Int
scoreCompletion (Incomplete remaining) = Just (foldl calculate 0 remaining)
  where
    calculate score ch =
      5 * score + case ch of
        '(' -> 1
        '[' -> 2
        '{' -> 3
        '<' -> 4
        _ -> 0
scoreCompletion _ = Nothing

checkSyntax :: Text.Text -> Syntax
checkSyntax chunks = case parse chunks of
  Parse [] -> Ok
  Parse remaining -> Incomplete remaining
  InvalidToken token -> Error token

parse :: Text.Text -> Parse
parse = Text.foldl matchToken (Parse [])
  where
    matchToken (Parse ('(' : cs)) ')' = Parse cs
    matchToken (Parse ('[' : cs)) ']' = Parse cs
    matchToken (Parse ('{' : cs)) '}' = Parse cs
    matchToken (Parse ('<' : cs)) '>' = Parse cs
    matchToken (Parse cs) token
      | token `elem` "([{<" = Parse (token : cs)
      | otherwise = InvalidToken token
    matchToken result _ = result
