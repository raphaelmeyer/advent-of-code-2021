-- Buoyancy Interchange Transmission System

module Day16.Bits where

import Control.Applicative (Alternative (some), (<|>))
import Data.Functor ((<&>))
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Util.Parser as Parser

data Bit = Zero | One deriving (Eq, Show)

type Transmission = [Bit]

type Version = Int

data Operation = Sum | Product | Min | Max | Greater | Less | Equal deriving (Eq, Show)

data Packet
  = Literal Version Int
  | Operator Version Operation [Packet]
  deriving (Eq, Show)

type TxParser a = Parser.Parser Transmission a

readInput :: String -> IO Text.Text
readInput filename = readFile filename <&> Text.pack

parseInput :: Text.Text -> Transmission
parseInput = concatMap nibble . Text.unpack
  where
    nibble hex = case hex of
      '0' -> [Zero, Zero, Zero, Zero]
      '1' -> [Zero, Zero, Zero, One]
      '2' -> [Zero, Zero, One, Zero]
      '3' -> [Zero, Zero, One, One]
      '4' -> [Zero, One, Zero, Zero]
      '5' -> [Zero, One, Zero, One]
      '6' -> [Zero, One, One, Zero]
      '7' -> [Zero, One, One, One]
      '8' -> [One, Zero, Zero, Zero]
      '9' -> [One, Zero, Zero, One]
      'A' -> [One, Zero, One, Zero]
      'B' -> [One, Zero, One, One]
      'C' -> [One, One, Zero, Zero]
      'D' -> [One, One, Zero, One]
      'E' -> [One, One, One, Zero]
      'F' -> [One, One, One, One]
      _ -> []

decode :: Transmission -> Packet
decode = fst . Maybe.fromJust . Parser.parse packet

evaluate :: Packet -> Int
evaluate (Literal _ n) = n
evaluate (Operator _ op ps) = op' . map evaluate $ ps
  where
    op' = case op of
      Sum -> sum
      Product -> product
      Min -> minimum
      Max -> maximum
      Greater -> gt
      Less -> lt
      Equal -> eq
    lt [a, b] = if a < b then 1 else 0
    lt _ = undefined
    gt = lt . reverse
    eq [a, b] = if a == b then 1 else 0
    eq _ = undefined

versionSum :: Packet -> Int
versionSum (Literal v _) = v
versionSum (Operator v _ ps) = v + (sum . map versionSum) ps

-- Parsing

-- packet

packet :: TxParser Packet
packet = literal <|> operator

version :: TxParser Version
version = value <$> bits 3

-- literal

literal :: TxParser Packet
literal = Literal <$> version <* litType <*> litValue

litType :: TxParser ()
litType = () <$ one <* zero <* zero

litValue :: TxParser Int
litValue = value <$> groups
  where
    groups = (zero *> bits 4) <|> ((++) <$ one <*> bits 4 <*> groups)

-- operator

operator :: TxParser Packet
operator = Operator <$> version <*> opType <*> (opPacketsType0 <|> opPacketsType1)

opType :: TxParser Operation
opType =
  (Sum <$ zero <* zero <* zero)
    <|> (Product <$ zero <* zero <* one)
    <|> (Min <$ zero <* one <* zero)
    <|> (Max <$ zero <* one <* one)
    <|> (Greater <$ one <* zero <* one)
    <|> (Less <$ one <* one <* zero)
    <|> (Equal <$ one <* one <* one)

opPacketsType0 :: TxParser [Packet]
opPacketsType0 = do
  n <- value <$ zero <*> bits 15
  ps <- bits n
  Parser.subparse (some packet) ps

opPacketsType1 :: TxParser [Packet]
opPacketsType1 = do
  n <- value <$ one <*> bits 11
  Parser.exactly n packet

-- bit handling

one :: TxParser Bit
one = satisfy (== One)

zero :: TxParser Bit
zero = satisfy (== Zero)

bits :: Int -> TxParser [Bit]
bits 0 = pure []
bits n = (:) <$> bit <*> bits (n -1)

satisfy :: (Bit -> Bool) -> TxParser Bit
satisfy p = Parser.P satisfy'
  where
    satisfy' [] = Nothing
    satisfy' (b : bs) = if p b then Just (b, bs) else Nothing

bit :: TxParser Bit
bit = Parser.P bit'
  where
    bit' [] = Nothing
    bit' (b : bs) = Just (b, bs)

-- helpers

value :: [Bit] -> Int
value = foldl addBit 0
  where
    addBit v One = 2 * v + 1
    addBit v _ = 2 * v
