module Parser where

import Control.Applicative

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) x = p x

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = P (\s -> case parse p s of
        []        -> []
        [(v, s')] -> [(f v, s')])

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure x = P (\s -> [(x, s)])
    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pf <*> px = P (\s -> case parse pf s of
        []        -> []
        [(f, s')] -> parse (f <$> px) s')

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\s -> case parse p s of
        []        -> []
        [(v, s')] -> parse (f v) s')

instance Alternative Parser where
    -- empty :: Parser a
    empty = P (const [])
    -- (<|>) :: Parser a -> Parser a -> Parser a
    p1 <|> p2 = P (\s -> case parse p1 s of
        []        -> parse p2 s
        [(v, s')] -> [(v, s')])
