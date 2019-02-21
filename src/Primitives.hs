module Primitives where

import Control.Applicative
import Data.Char

import Parser
import Language

item :: Parser Char
item = P (\s -> case s of
    []     -> []
    (x:xs) -> [(x, xs)])

sat :: (Char -> Bool) -> Parser Char
sat f = item >>= predicate where
    predicate c = if f c then return c else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

alpha :: Parser Char
alpha = sat isAlpha

alnum :: Parser Char
alnum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = char x *> string xs *> return (x:xs)

anyString :: [String] -> Parser String
anyString [] = empty
anyString (x:xs) = string x <|> anyString xs

name :: Parser String
name = (:) <$> alpha <*> many (alnum <|> char '_')

ident :: Parser String
ident = name >>= (\s -> if s `elem` keywords then empty else return s)

nat :: Parser Int
nat = read <$> some digit

space :: Parser ()
space = many (sat isSpace) *> return ()

-- Parses a list of at least one p delimited internally by d
delimList :: Parser a -> Parser b -> Parser [b]
delimList d p = (:) <$> p <*> many (d *> p)
