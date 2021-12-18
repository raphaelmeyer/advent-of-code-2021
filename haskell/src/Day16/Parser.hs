module Day16.Parser where

import Control.Applicative (Alternative (empty, (<|>)))

newtype Parser i a = P (i -> Maybe (a, i))

parse :: Parser i a -> i -> Maybe (a, i)
parse (P parser) = parser

instance Functor (Parser i) where
  -- fmap :: (a -> b) -> f a -> f b
  fmap g p = P $ \input -> case parse p input of
    Just (v, out) -> Just (g v, out)
    Nothing -> Nothing

instance Applicative (Parser i) where
  -- pure :: a -> f a
  pure v = P (\input -> Just (v, input))

  -- <*> :: f (a -> b) -> f a -> f b
  pg <*> px = P $ \input -> case parse pg input of
    Just (g, out) -> parse (fmap g px) out
    Nothing -> Nothing

instance Alternative (Parser i) where
  -- empty :: f a
  empty = P (const Nothing)

  -- (<|>) :: f a -> f a -> f a
  p <|> q = P $ \input -> case parse p input of
    Just (v, out) -> Just (v, out)
    Nothing -> parse q input

instance Monad (Parser i) where
  -- return :: a -> f a
  return = pure

  -- (>>=) :: f a -> (a -> f b) -> f b
  p >>= f = P $ \input -> case parse p input of
    Just (v, out) -> parse (f v) out
    Nothing -> Nothing

-- utilities

subparse :: Parser i a -> i -> Parser i a
subparse p input = case parse p input of
  Just (v, _) -> pure v
  _ -> empty

exactly :: Int -> Parser i a -> Parser i [a]
exactly 0 _ = pure []
exactly n p = (:) <$> p <*> exactly (n - 1) p
