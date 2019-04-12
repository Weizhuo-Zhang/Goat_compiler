module Main where

import GoatAST
import GoatParser
import GoatPrettyPrint
import Data.Char
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Q
import System.Environment
import System.Exit

checkArgs :: String -> [String] -> IO ()
checkArgs progName []
  = exitWithError ("Usage: " ++ progName ++ " [-p] fileName\n\n")
checkArgs progName (x:xs)
  = if "-p" == x then
      return ()
    else
      exitWithError ("Sorry, we have not impletement the compiler yet.\n" ++
                     "[ERROR] Usage: " ++ progName ++ " [-p] fileName\n\n")

main :: IO ()
main
  = do { progName <- getProgName
       ; args <- getArgs
       ; checkArgs progName args
       ; input <- readFile (args !! 1)
       ; let output = runParser pMain 0 "" input
       ; case output of
           Right ast -> prettyPrint ast -- print ast
           Left  err -> do { putStr "Parse error at "
                           ; print err
                           }
       }