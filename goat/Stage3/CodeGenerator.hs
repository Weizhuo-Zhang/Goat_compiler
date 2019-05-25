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
      ; generateStatements procName [0] (statementTable procedureTable)
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
    IfTable exprTable stmtTables ->
      generateIfStatement procName label exprTable stmtTables
    IfElseTable exprTable stmtTables1 stmtTables2 ->
      generateIfElseStatement procName label exprTable stmtTables1 stmtTables2
    WhileTable exprTable stmtTables ->
      generateWhileStatement procName label exprTable stmtTables
    -- TODO
    -- AssignTable
    -- ReadTable
    -- CallTable

generateWriteStatement :: ExpressionTable -> IO ()
generateWriteStatement exprTable =
    case exprTable of
        StringTable string -> do { printNewLineIndentation
                        ; putStr "string_const r0, "
                        ; putStrLn $ "\"" ++ string ++ "\""
                        ; printNewLineIndentation
                        ; putStrLn "call_builtin print_string"
                        }
        IntTable val -> do { printNewLineIndentation
                           ; putStrLn $ "int_const r0, " ++
                             (show $ val)
                           ; printNewLineIndentation
                           ; putStrLn "call_builtin print_int"
                           }
        FloatTable val -> do { printNewLineIndentation
                               ; putStrLn $ "real_const r0, " ++
                                 (show $ val)
                               ; printNewLineIndentation
                               ; putStrLn "call_builtin print_real"
                               }
        BoolTable bool -> do
            case bool of
                True -> do { printNewLineIndentation
                           ; putStrLn "int_const r0, 1"
                           ; printNewLineIndentation
                           ; putStrLn "call_builtin print_bool"
                           }
                False -> do { printNewLineIndentation
                            ; putStrLn "int_const r0, 0"
                            ; printNewLineIndentation
                            ; putStrLn "call_builtin print_bool"
                            }
        VariableTable var varType -> return ()
        otherwise -> do
           let exprType = getExprType exprTable
           generateExpression exprTable 0
           case exprType of
               IntType -> do { printNewLineIndentation
                             ; putStrLn "call_builtin print_int"
                             }
               FloatType -> do { printNewLineIndentation
                               ; putStrLn "call_builtin print_real"
                               }



-- generateReadStatement :: ExpressionTable -> IO ()
-- generateReadStatement exprTable = do {}

generateExpression :: ExpressionTable -> Int -> IO ()
generateExpression exprTable registerNum =
    case exprTable of
        IntTable val -> do { printNewLineIndentation
                           ; putStrLn $ "int_const r" ++ (show registerNum)
                             ++ ", " ++ (show val)
                           }
        FloatTable val -> do { printNewLineIndentation
                             ; putStrLn $ "real_const r" ++ (show registerNum)
                               ++ ", " ++ (show val)
                             }
        AddTable lExpr rExpr baseType -> do
              generateExpression lExpr registerNum
              generateExpression rExpr $ registerNum+1
              case baseType of
                   IntType -> do { generateOperationString "add" "int" registerNum }
                   FloatType -> do { generateIntToFloat lExpr rExpr registerNum
                                   ; generateOperationString "add" "real" registerNum
                                   }
        SubTable lExpr rExpr baseType -> do
              generateExpression lExpr registerNum
              generateExpression rExpr $ registerNum+1
              case baseType of
                   IntType -> do { generateOperationString "sub" "int" registerNum }
                   FloatType -> do { generateIntToFloat lExpr rExpr registerNum
                                   ; generateOperationString "sub" "real" registerNum
                                   }
        MulTable lExpr rExpr baseType -> do
              generateExpression lExpr registerNum
              generateExpression rExpr $ registerNum+1
              case baseType of
                   IntType -> do { generateOperationString "mul" "int" registerNum }
                   FloatType -> do { generateIntToFloat lExpr rExpr registerNum
                                   ; generateOperationString "mul" "real" registerNum
                                   }
        DivTable lExpr rExpr baseType -> do
              generateExpression lExpr registerNum
              generateExpression rExpr $ registerNum+1
              case baseType of
                   IntType -> do { generateOperationString "div" "int" registerNum }
                   FloatType -> do { generateIntToFloat lExpr rExpr registerNum
                                   ; generateOperationString "div" "real" registerNum
                                   }








updateLabel :: [Int] -> [Int]
updateLabel (x:[]) = (x+1):[]
updateLabel (x:xs) = x:(updateLabel xs)

showLabel :: [Int] -> String
showLabel (x:[]) = show(x)
showLabel (x:xs) = show(x) ++ "_" ++ showLabel(xs)

generateIfStatement ::
  String -> [Int] -> ExpressionTable -> [StatementTable] -> IO ()
generateIfStatement procName label exprTable stmts = do
  { let label_a = procName ++ "_" ++ (showLabel label) ++ "_a"
  ; let label_b = procName ++ "_" ++ (showLabel label) ++ "_b"
  -- check condition
  ; generateExpressionTable exprTable
  ; printLine ("branch_on_true r0, " ++ label_a)
  ; printLine ("branch_uncond " ++ label_b)
  -- If statements
  ; putStrLn (label_a ++ ":")
  ; generateStatements procName (label ++ [0] ++ [0]) stmts
  -- end of this statements
  ; putStrLn (label_b ++ ":")
  }

generateIfElseStatement ::
  String -> [Int] -> ExpressionTable -> [StatementTable] -> [StatementTable]
  -> IO ()
generateIfElseStatement procName label exprTable stmts1 stmts2 = do
  { let label_a = procName ++ "_" ++ (showLabel label) ++ "_a"
  ; let label_b = procName ++ "_" ++ (showLabel label) ++ "_b"
  ; generateExpressionTable exprTable
  -- Else statements
  ; printLine ("branch_on_false r0, " ++ label_a)
  -- If statements
  ; generateStatements procName (label ++ [1] ++ [0]) stmts1
  ; printLine ("branch_uncond " ++ label_b)
  -- Else statements
  ; putStrLn (label_a ++ ":")
  ; generateStatements procName (label ++ [2] ++ [0]) stmts2
  -- fi The end of If-Else
  ; putStrLn (label_b ++ ":")
  }

generateWhileStatement ::
  String -> [Int] -> ExpressionTable -> [StatementTable] -> IO ()
generateWhileStatement procName label exprTable stmts = do
  { let label_a = procName ++ "_" ++ (showLabel label) ++ "_a"
  ; let label_b = procName ++ "_" ++ (showLabel label) ++ "_b"
  ; let label_c = procName ++ "_" ++ (showLabel label) ++ "_c"
  -- check condition
  ; putStrLn (label_a ++ ":")
  ; generateExpressionTable exprTable
  ; printLine ("branch_on_true r0, " ++ label_b)
  ; printLine ("branch_uncond " ++ label_c)
  -- while statements
  ; putStrLn (label_b ++ ":")
  ; generateStatements procName (label ++ [3] ++ [0]) stmts
  -- check condition again
  ; printLine ("branch_uncond " ++ label_a)
  -- end of this while loop
  ; putStrLn (label_c ++ ":")
  }

generateExpressionTable :: ExpressionTable -> IO ()
generateExpressionTable exprTable =
  case exprTable of
    BoolTable val -> printLine ("int_const r0, " ++ (convertBoolToInt val))
    OrTable lExpr rExpr exprType ->
      generateOrExpression lExpr rExpr exprType
    AndTable lExpr rExpr exprType ->
      generateAndExpression lExpr rExpr exprType
-- TODO change this to register allocation
-- TODO
--    IntTable val -> printLine "int_const r0, " ++ val
--    FloatTable val -> printLine "real_const r0, " ++ val
--    StringTable val -> "string_const r0, " ++ val
--    EqTable lExpr rExpr exprType ->
--      generateEqExpression lExpr rExpr exprType
--    NotEqTable lExpr rExpr exprType ->
--      generateNotEqExpression lExpr rExpr exprType
--    LesTable lExpr rExpr exprType ->
--      generateLesExpression lExpr rExpr exprType
--    LesEqTable lExpr rExpr exprType ->
--      generateLesEqExpression lExpr rExpr exprType
--    GrtTable lExpr rExpr exprType ->
--      generateGrtExpression lExpr rExpr exprType
--    GrtEqTable lExpr rExpr exprType ->
--      generateGrtEqExpression lExpr rExpr exprType
    NotTable expr exprType ->
      generateNotExpression expr exprType

generateOrExpression :: ExpressionTable -> ExpressionTable -> BaseType -> IO ()
generateOrExpression lExpr rExpr exprType = do
-- TODO change this to register allocation
  { generateExpressionTable lExpr
  ; printLine "move r1, r0"
  ; generateExpressionTable rExpr
  ; printLine "or r0, r0, r1"
  }

generateAndExpression :: ExpressionTable -> ExpressionTable -> BaseType -> IO ()
generateAndExpression lExpr rExpr exprType = do
-- TODO change this to register allocation
  { generateExpressionTable lExpr
  ; printLine "move r1, r0"
  ; generateExpressionTable rExpr
  ; printLine "and r0, r0, r1"
  }

generateNotExpression :: ExpressionTable -> BaseType -> IO ()
generateNotExpression expr exprType = do
  { generateExpressionTable expr
  ; printLine "not r0, r0"
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

getExprType :: ExpressionTable -> BaseType
getExprType exprTable =
     case exprTable of
          IntTable _ -> IntType
          FloatTable _ -> FloatType
          BoolTable _ -> BoolType
          AddTable _ _ baseType -> baseType
          SubTable _ _ baseType -> baseType
          MulTable _ _ baseType -> baseType
          DivTable _ _ baseType -> baseType

generateOperationString :: String -> String -> Int -> IO ()
generateOperationString operator opType registerNum = do
  printNewLineIndentation
  putStrLn $ operator ++ "_" ++ opType ++ " r" ++ (show registerNum)
    ++ ", r" ++ (show registerNum) ++ ", r" ++
    (show $ registerNum+1)

generateIntToFloat :: ExpressionTable -> ExpressionTable -> Int -> IO ()
generateIntToFloat lExpr rExpr registerNum = do
    let lType = getExprType lExpr
        rType = getExprType rExpr
    case (lType,rType) of
        (FloatType,FloatType) -> return ()
        (IntType,FloatType) -> do { printNewLineIndentation
                                 ; putStrLn $ "int_to_real r" ++ (show registerNum)
                                   ++ ", r" ++ (show registerNum)
                                 }
        (FloatType,IntType) -> do { printNewLineIndentation
                                 ; putStrLn $ "int_to_real r" ++ (show $ registerNum+1)
                                   ++ ", r" ++ (show $ registerNum+1)
                                 }
