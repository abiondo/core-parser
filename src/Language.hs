module Language where

-- Variable name
type Name = String

-- Let(rec) definition (var, body)
type Def a = (a, Expr a)
type CoreDef = Def Name

-- Case alternative (tag, [params], body)
type Alter a = (Int, [a], Expr a)
type CoreAlter = Alter Name

-- Is let(rec) recursive?
data IsRec = NonRecursive | Recursive deriving (Eq, Show)

-- Expression
data Expr a =
      EVar Name                   -- Variable
    | ENum Int                    -- Number
    | EConstr Int Int             -- Constructor (tag arity)
    | EAp (Expr a) (Expr a)       -- Application (lhs rhs)
    | ELet IsRec [Def a] (Expr a) -- Let/letrec (rec? [defs] body)
    | ECase (Expr a) [Alter a]    -- Case (expr [alters])
    | ELam [a] (Expr a)           -- Lambda ([params] body)
    deriving (Eq, Show)
type CoreExpr = Expr Name

-- Supercombinator (var, [params], body)
type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

-- Program
type Program a = [ScDefn a]
type CoreProgram = Program Name

-- Reserved keywords
keywords = ["Pack", "let", "letrec", "case", "in", "of"]
