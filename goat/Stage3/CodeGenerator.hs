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
      ; generateStatements procName [0] (statements procedureTable)
      ; printNewLineIndentation
      ; putStrLn "return"
      }
    Nothing -> putStrLn "Main not found"
  where procName = "proc_main"

generateStatements :: String -> [Int] -> [StatementTable] -> IO ()
generateStatements _ _ [] = return ()
generateStatements procName label (stat:[]) = do
  generateStatement procName (updateLabel label) stat

generateStatements procName label (stat:stats) = do
  { generateStatement procName (updateLabel label) stat
  ; generateStatements procName (updateLabel label) stats
  }

generateStatement :: String -> [Int] -> StatementTable -> IO ()
generateStatement procName label statementTable = do
    case statementTable of
        WriteTable exprTable -> do { generateWriteStatement exprTable }
        -- TODO
        IfTable exprTable stmtTables -> do
            { generateIfStatement procName label exprTable stmtTables}

generateWriteStatement :: ExpressionTable -> IO ()
generateWriteStatement exprTable =
    case exprTable of
        StringTable string -> do { printNewLineIndentation
                        ; putStr "string_const r0, "
                        ; putStrLn $ "\"" ++ string ++ "\""
                        ; printNewLineIndentation
                        ; putStrLn "call_builtin print_string"
                        }

updateLabel :: [Int] -> [Int]
updateLabel (x:[]) = (x+1):[]
updateLabel (x:xs) = x:(updateLabel xs)

showLabel :: [Int] -> String
showLabel (x:[]) = show(x)
showLabel (x:xs) = show(x) ++ "_" ++ showLabel(xs)

-- TODO
generateIfStatement :: String -> [Int] -> ExpressionTable -> [StatementTable] -> IO ()
generateIfStatement procName label exprTable stmts = do
  { let label_a = procName ++ "_" ++ (showLabel label) ++ "_a"
  ; let label_b = procName ++ "_" ++ (showLabel label) ++ "_b"
  ; generateExpressionTable exprTable
  ; printLine ("branch_on_true r0, " ++ label_a)
  ; printLine ("branch_uncond " ++ label_b)
  ; putStrLn (label_a ++ ":")
  ; generateStatements procName (label ++ [0]) stmts
  ; putStrLn (label_b ++ ":")
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
    True  -> show 1
    False -> show 0

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
