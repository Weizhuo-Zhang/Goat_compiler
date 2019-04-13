module Main where

import GoatAST
import GoatParser
import GoatPrettyPrint
import GoatExit
import System.Exit
import Text.Parsec (runParser)
import System.Environment (getArgs, getProgName)

checkArgs :: String -> [String] -> IO Task
checkArgs _ ['-':_] = exitWithError "Missing filename" MissingFile
checkArgs _ [filename] = return Compile
checkArgs _ ["-p", filename] = return Pprint
checkArgs _ ["-a", filename] = return Parse
checkArgs progname _  = exitWithError ("Usage: " ++ progname ++ " [-p] filename") MissingFile

main :: IO ()
main
  = do
     progname <- getProgName
     args <- getArgs
     task <- checkArgs progname args
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
             Right ast -> putStrLn (show ast) -- print ast
             Left  err -> do {  putStr "Parse error at "
                             ; print err
                             ; exitWith (ExitFailure 2)
                             }
       else
         do
           let [_, filename] = args
           input <- readFile filename
           -- let output = ast input
           let output = runParser pMain 0 "" input
           case output of
             Right ast -> prettyPrint ast -- print ast
             Left  err -> do {  putStr "Parse error at "
                             ; print err
                             ; exitWith (ExitFailure 2)
                             }
