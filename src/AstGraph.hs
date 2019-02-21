module AstGraph where

import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.GraphViz.Types as G

import qualified Data.Text.Lazy as TL

import Language

type VLabel = ()
type V = (String, VLabel)

type ELabel = ()
type E = (String, String, ELabel)

type AstGraph = ([V], [E])

type GraphBuilderState = (Int, AstGraph)
newtype GraphBuilder a = GB (GraphBuilderState -> (a, GraphBuilderState))

instance Functor GraphBuilder where
    -- fmap :: (a -> b) -> GraphBuilder a -> GraphBuilder b
    fmap f b = GB (\s -> let (v, s') = build b s in (f v, s'))

instance Applicative GraphBuilder where
    -- pure :: a -> GraphBuilder a
    pure x = GB (\s -> (x, s))
    -- (<*>) :: GraphBuilder (a -> b) -> GraphBuilder a -> GraphBuilder b
    bf <*> bx = GB (\s -> let (f, s') = build bf s in build (f <$> bx) s')

instance Monad GraphBuilder where
    -- (>>=) :: GraphBuilder a -> (a -> GraphBuilder b) -> GraphBuilder b
    b >>= f = GB (\s -> let (v, s') = build b s in build (f v) s')

empty :: GraphBuilderState
empty = (0, ([], []))

build :: GraphBuilder a -> GraphBuilderState -> (a, GraphBuilderState)
build (GB f) s = f s

evalGraph :: GraphBuilder a -> GraphBuilderState -> AstGraph
evalGraph b s = let (_, (_, g)) = build b s in g

addNode :: String -> GraphBuilder String
addNode desc = GB (\s ->
    let (n, (vs, es)) = s
        node = (show n) ++ ": " ++ desc
        vs' = vs ++ [(node, ())]
        s' = (n+1, (vs', es))
    in (node, s'))

addEdge :: String -> String -> GraphBuilder ()
addEdge from to = GB (\s ->
    let (n, (vs, es)) = s
        es' = es ++ [(from, to, ())]
        s' = (n, (vs, es'))
    in ((), s'))

addDef :: String -> CoreDef -> GraphBuilder ()
addDef parent (var, body) = do
    n <- addNode $ "Def " ++ var
    addEdge parent n
    addExpr n body

addAlter :: String -> CoreAlter -> GraphBuilder ()
addAlter parent (tag, params, body) = do
    n <- addNode $ "Alter <" ++ show tag ++ "> " ++ unwords params
    addEdge parent n
    addExpr n body

addExpr :: String -> CoreExpr -> GraphBuilder ()
addExpr parent expr = (
    case expr of
        EVar x          -> addNode $ "EVar " ++ x
        e@(ENum _)      -> addNode $ show e
        e@(EConstr _ _) -> addNode $ show e
        EAp lhs rhs     -> do n <- addNode "EAp"
                              addExpr n lhs
                              addExpr n rhs
                              return n
        ELet r ds e     -> do n <- addNode $ "ELet " ++ show r
                              mapM (addDef n) ds
                              addExpr n e
                              return n
        ECase e as      -> do n <- addNode "ECase"
                              addExpr n e
                              mapM (addAlter n) as
                              return n
        ELam ps e       -> do n <- addNode $ "ELam " ++ unwords ps
                              addExpr n e
                              return n
    ) >>= addEdge parent

addScDefn :: String -> CoreScDefn -> GraphBuilder ()
addScDefn parent (var, params, body) = do
    n <- addNode $ "ScDefn " ++ var ++ " " ++ unwords params
    addEdge parent n
    addExpr n body

addProgram :: CoreProgram -> GraphBuilder ()
addProgram scs = do
    n <- addNode "Program"
    mapM (addScDefn n) scs
    return ()

astGraphParams :: G.GraphvizParams String VLabel ELabel () VLabel
astGraphParams = G.defaultParams

mkDotAstGraph :: CoreProgram -> String
mkDotAstGraph prog = let (vs, es) = evalGraph (addProgram prog) empty
                         dotGraph = G.graphElemsToDot astGraphParams vs es :: G.DotGraph String
                         dotText = G.printDotGraph dotGraph :: TL.Text
                     in TL.unpack dotText
