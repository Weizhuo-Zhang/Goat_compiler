module CodeGenerator where

import GoatAST
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
                               ; putStr "call proc_main"
                               ; printNewLineIndentation
                               ; putStr "halt"
                               ; case Map.lookup "main" programMap of
                                    
                               }

generateMain :: ProcedureTable -> IO ()
generateMain procedureTable = do { generateStatements (statements procedureTable)
                                  }

generateStatements :: [Statement] -> IO ()
generateStatements [] = return ()
generateStatements (stat:[]) = do {
                               ; printNewLineIndentation
                               ; putStr "return"
                               }

generateStatement :: Statement -> IO ()
generateStatement statement = case statement of
  Write expression -> generateWriteStatement expression

generateWriteStatement :: Expression -> IO ()
generateWriteStatement expression = case expression of
  StrConst string -> do { printNewLineIndentation
                        ; putStr "string_const r0, "
                        ; putStr string
                        ; printNewLineIndentation
                        ; putStr "call_builtin print_string"
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
printNewLineIndentation = putStrLn "    "
