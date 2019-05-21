module Main where

import GoatAST
import GoatParser
import GoatPrettyPrint
import GoatExit
import Analyze
import CodeGenerator
import Text.Parsec (runParser)
import System.Environment (getArgs, getProgName)

-------------------------------- Documentation --------------------------------

-- Authors:
--   Shizhe Cai (shizhec) - 798125
--   Weizhuo Zhang (weizhuoz) - 1018329
--   Mingyang Zhang (mingyangz) - 650242
--   An Luo (aluo1) - 657605

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
checkArgs _ ["-s", filename] = return Analyze
checkArgs progname _         = do
  exitWithError ("Usage: " ++ progname ++ " [-ap] filename") WrongUsage

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
         let [filename] = args
         input <- readFile filename
         let output = runParser pMain 0 "" input
         case output of
           Right ast -> codeGeneration $ semanticAnalyse ast
           Left  err -> do { exitWithError ("Parse error at " ++ show(err)) ParseError
                           ; return ()
                           }
         exitWithSuccess "Compiled Complete"
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
         if task == Analyze then
           do
             let [_, filename] = args
             input <- readFile filename
             let output = runParser pMain 0 "" input
             case output of
               Right ast -> do { let programMap = semanticAnalyse ast
                               ; putStrLn $ show programMap
                               ; return ()
                               }
               Left err -> do { exitWithError ("Parse error at " ++ show(err)) ParseError
                               ; return ()
                               }
         else
           do
             let [_, filename] = args
             input <- readFile filename
             -- let output = ast input
             let output = runParser pMain 0 "" input
             case output of
               Right ast -> prettyPrint ast -- pretty print ast
               Left  err -> do { exitWithError ("Parse error at " ++ show(err)) ParseError
                               ; return ()
                               }
