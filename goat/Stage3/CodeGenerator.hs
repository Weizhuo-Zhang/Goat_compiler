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
    Just procedureTable -> do
      { putStrLn (procName ++ ":")
      ; generateStatements procName 0 (statements procedureTable)
      }
    Nothing -> putStrLn "Main not found"
  where procName = "proc_main"

generateStatements :: String -> Int -> [StatementTable] -> IO ()
generateStatements _ _ [] = return ()
generateStatements procName label (stat:[]) = do
  { generateStatement procName label+1 stat
  ; printNewLineIndentation
  ; putStrLn "return"
  }
generateStatements procName label (stat:stats) = do
  { generateStatement procName label+1 stat
  ; generateStatements procName label+1 stats
  }

generateStatement :: String -> Int -> StatementTable -> IO ()
generateStatement procName label statementTable = do
    case statementTable of
        WriteTable exprTable -> do { generateWriteStatement exprTable }
        -- TODO
        IfTable exprTable stmtTables -> do
            { generateIfStatement procName (label+2) exprTable stmtTables}

generateWriteStatement :: ExpressionTable -> IO ()
generateWriteStatement exprTable =
    case exprTable of
        StringTable string -> do { printNewLineIndentation
                        ; putStr "string_const r0, "
                        ; putStrLn $ "\"" ++ string ++ "\""
                        ; printNewLineIndentation
                        ; putStrLn "call_builtin print_string"
                        }

-- TODO
generateIfStatement :: String -> Int -> ExpressionTable -> [StatementTable] -> IO ()
generateIfStatement procName label exprTable stmts = do
  { let label_num = label Map.! procName
  ; let label_0 = procName ++ "_" ++ show(label_num - 1)
  ; let label_1 = procName ++ "_" ++ show(label_num)
  ; generateExpressionTable exprTable
  ; printLine ("branch_on_true, r0, " ++ label_0)
  ; printLine ("branch_uncond, " ++ label_1)
  ; putStrLn (label_0 ++ ":")
  ; generateStatements procName label stmts
  ; putStrLn (label_1 ++ ":")
  }

-- TODO
generateExpressionTable :: ExpressionTable -> IO ()
generateExpressionTable exprTable =
  case exprTable of
    -- TODO
--    SingleExprTable expr exprType ->
--        generateSingleExpression expr exprTable
    OrTable lExpr rExpr exprType ->
        generateOrExpression lExpr rExpr exprType
    AndTable lExpr rExpr exprType ->
        generateAndExpression lExpr rExpr exprType
    BoolTable val -> printLine ("int_const r0, " ++ (convertBoolToInt val))
--    IntTable val -> printLine "int_const r0, " ++ val
--    FloatTable val -> printLine "real_const r0, " ++ val
--    StringTable val -> "string_const r0, " ++ val

-- TODO
--generateSingleExpression :: Expression -> BaseType -> IO ()
--generateSingleExpression expr exprType =
--    case expr of

generateOrExpression :: ExpressionTable -> ExpressionTable -> BaseType -> IO ()
generateOrExpression lExpr rExpr exprType = do
  { generateExpressionTable lExpr
  ; printLine "move r1, r0"
  ; generateExpressionTable rExpr
  ; printLine "or r0, r0, r1"
  }

generateAndExpression :: ExpressionTable -> ExpressionTable -> BaseType -> IO ()
generateAndExpression lExpr rExpr exprType = do
  { generateExpressionTable lExpr
  ; printLine "move r1, r0"
  ; generateExpressionTable rExpr
  ; printLine "and r0, r0, r1"
  }

convertBoolToInt :: Bool -> String
convertBoolToInt boolVal =
  case boolVal of
    True  -> show 0
    False -> show 1

printLine :: String -> IO ()
printLine string = do
  { printNewLineIndentation
  ; putStrLn string
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
