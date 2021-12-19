module Day18.Parser where

import Control.Applicative (Alternative (some, (<|>)))
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Day18.Number as S
import qualified Util.Parser as Parser

type SfParser a = Parser.Parser String a

parseNumber :: Text.Text -> S.Number
parseNumber input = case result of
  Just (n, _) -> n
  _ -> undefined
  where
    result = Parser.parse pair . Text.unpack $ input

pair :: SfParser S.Number
pair = S.Pair <$ char '[' <*> number <* char ',' <*> number <* char ']'

number :: SfParser S.Number
number = regular <|> pair

regular :: SfParser S.Number
regular = S.Regular <$> int

char :: Char -> SfParser Char
char ch = satisfy (== ch)

int :: SfParser Int
int = read <$> some digit

digit :: SfParser Char
digit = satisfy Char.isDigit

satisfy :: (Char -> Bool) -> SfParser Char
satisfy p = Parser.P satisfy'
  where
    satisfy' [] = Nothing
    satisfy' (b : bs) = if p b then Just (b, bs) else Nothing