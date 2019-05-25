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
    Just procedureTable -> do { putStrLn "proc_main:"
                              ; generateStatements $ statementTable procedureTable
                              }
    Nothing -> putStrLn "Main not found"

generateStatements :: [StatementTable] -> IO ()
generateStatements [] = return ()
generateStatements (stat:[]) = do { generateStatement stat
                                  ; printNewLineIndentation
                                  ; putStrLn "return"
                                  }
generateStatements (stat:stats) = do { generateStatement stat
                                     ; generateStatements stats
                                     }

generateStatement :: StatementTable -> IO ()
generateStatement statementTable = do
    let stmt = statement statementTable
        exprTable = expressionTable statementTable
    case stmt of
        Write expression -> do { generateWriteStatement exprTable }
        -- Read variable -> do { generateReadStatement exprTable }

generateWriteStatement :: ExpressionTable -> IO ()
generateWriteStatement exprTable =
    case exprTable of
        StringTable string -> do { printNewLineIndentation
                        ; putStr "string_const r0, "
                        ; putStrLn $ "\"" ++ string ++ "\""
                        ; printNewLineIndentation
                        ; putStrLn "call_builtin print_string"
                        }
        IntTable int -> do { printNewLineIndentation
                           ; putStrLn $ "int_const r0, " ++
                             (show $ int)
                           ; printNewLineIndentation
                           ; putStrLn "call_builtin print_int"
                           }
        FloatTable float -> do { printNewLineIndentation
                               ; putStrLn $ "real_const r0, " ++
                                 (show $ float)
                               ; printNewLineIndentation
                               ; putStrLn "call_builtin print_real"
                               }
        BoolTable bool -> do
            case bool of
                True -> do { printNewLineIndentation
                           ; putStrLn $ "int_const r0, 1"
                           ; printNewLineIndentation
                           ; putStrLn "call_builtin print_bool"
                           }
                False -> do { printNewLineIndentation
                            ; putStrLn $ "int_const r0, 0"
                            ; printNewLineIndentation
                            ; putStrLn "call_builtin print_bool"
                            }
        VariableTable var varType -> return ()
        -- otherwise -> generateExpression exprTable 0


-- generateReadStatement :: ExpressionTable -> IO ()
-- generateReadStatement exprTable = do {}

-- generateExpression :: ExpressionTable -> Int -> IO ()
-- generateExpression exprTable registerNum =
--     case exprTable of
--         AddTable  -> expression
--       otherwise -> expression
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
