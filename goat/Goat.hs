module Main where

import GoatAST
import GoatParser
import GoatPrettyPrint
import GoatExit
import Text.Parsec (runParser)
import System.Environment (getArgs, getProgName)

-------------------------------- Documentation --------------------------------

-- This is the main file for Stage 1 of the project of COMP90045, Programming
-- Language Implementation. This file provides two main methods: checkArgs and
-- main.

-- The aim of the project is to implement a compiler for a procedural (C-like)
-- language called Goat.

-------------------------------- Documentation --------------------------------

-------------------------------------------------------------------------------
-- Given the program name and arguments list, check if it's valid, thrown
-- exception if it's not valid, otherwise return the corresponding Task value.
-------------------------------------------------------------------------------
checkArgs :: String -> [String] -> IO Task
checkArgs _ ['-':_]          = exitWithError "Missing filename" MissingFile
checkArgs _ [filename]       = return Compile
checkArgs _ ["-p", filename] = return Pprint
checkArgs _ ["-a", filename] = return Parse
checkArgs progname _         = do
  exitWithError ("Usage: " ++ progname ++ " [-p] filename") WrongUsage

-------------------------------------------------------------------------------
-- Main function of the compiler
-------------------------------------------------------------------------------
main :: IO ()
main
  = do
     progname <- getProgName
     args     <- getArgs
     task     <- checkArgs progname args
     if task == Compile then
       do
         exitWithSuccess "Sorry, cannot generate code yet"
     else
       if task == Parse then
         do
           let [_, filename] = args
           input <- readFile filename
           let output = runParser pMain 0 "" input
           case output of
             Right ast -> print ast -- print ast
             Left  err -> do { exitWithError ("Parse error at " ++ show(err)) ParseError
                             ; return ()
                             }
       else
         do
           let [_, filename] = args
           input <- readFile filename
           -- let output = ast input
           let output = runParser pMain 0 "" input
           case output of
             Right ast -> prettyPrint ast -- print ast
             Left  err -> do { exitWithError ("Parse error at " ++ show(err)) ParseError
                             ; return ()
                             }
