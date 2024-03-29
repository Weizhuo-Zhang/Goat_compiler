module Main where

import           Analyzer
import           CodeGenerator
import           GoatAST
import           GoatExit
import           GoatParser
import           GoatPrettyPrint
import           MainAnalyzer
import           System.Environment (getArgs, getProgName)
import           Text.Parsec        (runParser)

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
           Right ast -> do { checkMainProc ast
                           ; let programMap = semanticAnalyse ast
                           ; case programMap of
                                Left err -> do
                                    { err
                                    ; return ()
                                    }
                                Right result -> do
                                    { codeGeneration result
                                    ; return ()
                                    }
                           }
           Left  err -> do
             { exitWithError ("Parse error at " ++ show(err)) ParseError
             ; return ()
             }
         exitWithSuccess
     else
       if task == Parse then
         do
           let [_, filename] = args
           input <- readFile filename
           let output = runParser pMain 0 "" input
           case output of
             Right ast -> print ast -- print ast
             Left  err -> do
               { exitWithError ("Parse error at " ++ show(err)) ParseError
               ; return ()
               }
       else
         if task == Analyze then
           do
             let [_, filename] = args
             input <- readFile filename
             let output = runParser pMain 0 "" input
             case output of
               Right ast -> do { checkMainProc ast
                               ; let programMap = semanticAnalyse ast
                               ; case programMap of
                                    Left err -> do
                                        { err
                                        ; return ()
                                        }
                                    Right result -> do
                                        { putStrLn $ show result
                                        ; return ()
                                        }
                               }
               Left err -> do
                 { exitWithError ("Parse error at " ++ show(err)) ParseError
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
               Left  err -> do
                 { exitWithError ("Parse error at " ++ show(err)) ParseError
                 ; return ()
                 }
