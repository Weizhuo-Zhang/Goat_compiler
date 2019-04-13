module Main where

import GoatAST
import GoatParser
import GoatPrettyPrint
import GoatExit
import Text.Parsec (runParser)
import System.Environment (getArgs, getProgName)

-- checkArgs :: String -> [String] -> IO ()
-- checkArgs progName []
--   = exitWithError ("Usage: " ++ progName ++ " [-p] fileName\n\n")
-- checkArgs progName (x:xs)
--   = if "-p" == x then
--       return ()
--     else
--       exitWithError ("Sorry, we have not impletement the compiler yet.\n" ++
--                      "[ERROR] Usage: " ++ progName ++ " [-p] fileName\n\n")

checkArgs :: String -> [String] -> IO Task
checkArgs _ ['-':_] = exitWithError "Missing filename" MissingFile
checkArgs _ [filename] = return Compile
checkArgs _ ["-p", filename] = return Pprint
checkArgs _ ["-a", filename] = return Parse
checkArgs progname _  = exitWithError ("Usage: " ++ progname ++ " [-p] filename") WrongUsage


-- main :: IO ()
-- main
--   = do { progName <- getProgName
--        ; args <- getArgs
--        ; putStrLn (head $ tail args)
--        ; task <- checkArgs progName args
--        ; input <- readFile (args !! 1)
--        ; let output = runParser pMain 0 "" input
--        ; case output of
--            Right ast -> prettyPrint ast -- print ast
--            Left  err -> do { putStr "Parse error at "
--                            ; print err
--                            }
--        }

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
           -- let output = ast input
           -- case output of
           --   Right tree -> putStrLn (show tree)
           --   Left err -> do { putStr "Parse error at "
           --                  ; print err
           --                  ; exitWith (ExitFailure 2)
           --                  }
           let output = runParser pMain 0 "" input
           case output of
             Right ast -> prettyPrint ast -- print ast
          --            Left  err -> do { putStr "Parse error at "
          --                            ; print err
          --                            }
       else
         do
           let [_, filename] = args
           input <- readFile filename
           -- let output = ast input
           let output = runParser pMain 0 "" input
           case output of
             Right ast -> prettyPrint ast -- print ast
          --            Left  err -> do { putStr "Parse error at "
          --                            ; print err
          --                            }

           -- Right tree -> putStrLn (prettyPrint tree)
           --   Left err -> do { putStr "Parse error at "
           --                  ; print err
           --                  ; exitWith (ExitFailure 2)
           --                  }
