{-# LANGUAGE DataKinds #-}

module Main where

import System.IO
import Options.Declarative
import Control.Monad.IO.Class (liftIO)

import Parser
import Language
import CoreParser
import AstGraph

comp :: [(Program Name, Name)] -> Program Name
comp [] = error "Parsing failed"
comp [(e, [])] = e
comp [(_, a)] = error $ "Leftover input: " ++ a

main' :: Flag "g" '["graph"] "false" "produce DOT output" Bool
      -> Arg "file" String
      -> Cmd "Core Language Parser" ()

main' graph file = do
    input <- liftIO . readFile $ get file
    let prog = comp $ parse parseProgram input
    liftIO . putStrLn $ if get graph then mkDotAstGraph prog else show prog

main :: IO ()
main = run_ main'
