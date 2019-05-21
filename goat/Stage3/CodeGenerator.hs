module CodeGenerator where

import GoatAST
import GoatExit
import SymbolTable
import qualified Data.Map.Strict as Map
import GoatPrettyPrint
import Control.Monad.State
-------------------------------- Documentation --------------------------------

-- Authors:
--   Shizhe Cai (shizhec) - 798125
--   Weizhuo Zhang (weizhuoz) - 1018329
--   Mingyang Zhang (mingyangz) - 650242
--   An Luo (aluo1) - 657605

-- This file contains the code generator of the Goat program.

-- The aim of the project is to implement a compiler for a procedural (C-like)
-- language called Goat.

-------------------------------- Documentation --------------------------------

type Register = String
data InputValue = Int | Float | String

type SlotNumber = Int
-------------------------------------------------------------------------------

codeGeneration :: ProgramMap -> IO ()
codeGeneration programMap = do { printNewLineIndentation
                               ; putStrLn "call proc_main"
                               ; printNewLineIndentation
                               ; putStrLn "halt"
                               ; generateMain programMap
                               }

generateMain :: ProgramMap -> IO ()
generateMain programMap =
  case Map.lookup "main" programMap of
    Just procedureTable -> do { putStrLn "main_proc:"
                              ; generateStatements $ statements procedureTable
                              }
    Nothing -> putStrLn "Main not found"

generateStatements :: [Statement] -> IO ()
generateStatements [] = return ()
generateStatements (stat:[]) = do { generateStatement stat
                                  ; printNewLineIndentation
                                  ; putStrLn "return"
                                  }
generateStatements (stat:stats) = do { generateStatement stat
                                     ; generateStatements stats
                                     }

generateStatement :: Statement -> IO ()
generateStatement statement = case statement of
  Write expression -> do { generateWriteStatement expression }

generateWriteStatement :: Expression -> IO ()
generateWriteStatement expression = case expression of
  StrConst string -> do { printNewLineIndentation
                        ; putStr "string_const r0, "
                        ; putStrLn string
                        ; printNewLineIndentation
                        ; putStrLn "call_builtin print_string"
                        }


-------------------------------------------------------------------------------
-- Register
-------------------------------------------------------------------------------
registers :: Map.Map register inputValue
registers = Map.empty






-------------------------------------------------------------------------------
-- Stack
-------------------------------------------------------------------------------





-------------------------------------------------------------------------------
-- helper functions
-------------------------------------------------------------------------------
printNewLineIndentation :: IO ()
printNewLineIndentation = putStr "    "
