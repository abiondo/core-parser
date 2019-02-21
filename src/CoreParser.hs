module CoreParser where

import Control.Applicative

import Parser
import Primitives
import Tokens
import Language

-- Application chain
apChain :: [CoreExpr] -> CoreExpr
apChain (f:xs) = foldl EAp f xs

semiList :: Parser a -> Parser [a]
semiList = delimList (symbol ";")

parseDef :: Parser CoreDef
parseDef = (,)
    <$> identifier                -- var
    <*> (symbol "=" *> parseExpr) -- body

parseAlter :: Parser CoreAlter
parseAlter = (,,)
    <$> (symbol "<" *> natural)         -- tag
    <*> (symbol ">" *> many identifier) -- [params]
    <*> (symbol "->" *> parseExpr)      -- body

parseConstr :: Parser CoreExpr
parseConstr = EConstr
    <$> (symbol "Pack" *> symbol "{" *> natural) -- tag
    <*> (symbol "," *> natural <* symbol "}")   -- arity

parseLetKeyword :: Parser IsRec
parseLetKeyword =
        (symbol "letrec" *> return Recursive)
    <|> (symbol "let" *> return NonRecursive)

parseLet :: Parser CoreExpr
parseLet = ELet
    <$> parseLetKeyword
    <*> semiList parseDef
    <*> (symbol "in" *> parseExpr)

parseCase :: Parser CoreExpr
parseCase = ECase
    <$> (symbol "case" *> parseExpr)         -- expr
    <*> (symbol "of" *> semiList parseAlter) -- alters

parseLambda :: Parser CoreExpr
parseLambda = ELam
    <$> (symbol "\\" *> some identifier) -- vars
    <*> (symbol "." *> parseExpr)        -- body

-- Takes the parsers to use for subexpressions
parseBinOp :: Parser CoreExpr -> Parser CoreExpr -> [String] -> Parser CoreExpr
parseBinOp plhs prhs ops = do
    lhs <- plhs
    op <- anySymbol ops
    rhs <- prhs
    return $ EAp (EAp (EVar op) lhs) rhs

parseParensExpr :: Parser CoreExpr
parseParensExpr = symbol "(" *> parseExpr <* symbol ")"

parseAExpr :: Parser CoreExpr
parseAExpr =
        EVar <$> identifier
    <|> ENum <$> natural
    <|> parseConstr
    <|> parseParensExpr

parseExpr :: Parser CoreExpr
parseExpr =
        parseLet
    <|> parseCase
    <|> parseLambda
    <|> parseExpr1

parseExpr1 :: Parser CoreExpr
parseExpr1 = 
        parseBinOp parseExpr2 parseExpr1 ["|"] -- Right-associative
    <|> parseExpr2

parseExpr2 :: Parser CoreExpr
parseExpr2 =
        parseBinOp parseExpr3 parseExpr2 ["&"] -- Right-associative
    <|> parseExpr3

parseExpr3 :: Parser CoreExpr
parseExpr3 =
        parseBinOp parseExpr4 parseExpr4 ["==", "~=", ">", ">=", "<", "<="] -- Non-associative
    <|> parseExpr4

parseExpr4 :: Parser CoreExpr
parseExpr4 =
        parseBinOp parseExpr5 parseExpr4 ["+"] -- Right-associative
    <|> parseBinOp parseExpr5 parseExpr5 ["-"] -- Non-associative
    <|> parseExpr5

parseExpr5 :: Parser CoreExpr
parseExpr5 =
        parseBinOp parseExpr6 parseExpr5 ["*"] -- Right-associative
    <|> parseBinOp parseExpr6 parseExpr6 ["/"] -- Non-associative
    <|> parseExpr6

parseExpr6 :: Parser CoreExpr
parseExpr6 = apChain <$> some parseAExpr -- Left-associative

parseScDefn :: Parser CoreScDefn
parseScDefn = (,,)
    <$> identifier                -- var
    <*> many identifier           -- [params]
    <*> (symbol "=" *> parseExpr) -- body

parseProgram :: Parser CoreProgram
parseProgram = semiList parseScDefn
