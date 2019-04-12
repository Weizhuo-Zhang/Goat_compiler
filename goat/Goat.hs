module Main where

import GoatAST
import GoatParser
import GoatPrettyPrint
import Text.Parsec (runParser)
import System.Environment (getArgs, getProgName)

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
