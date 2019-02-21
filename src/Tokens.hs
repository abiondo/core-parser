module Tokens where

import Parser
import Primitives

token :: Parser a -> Parser a
token p = space *> p <* space

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol x = token (string x)

anySymbol :: [String] -> Parser String
anySymbol xs = token (anyString xs)
