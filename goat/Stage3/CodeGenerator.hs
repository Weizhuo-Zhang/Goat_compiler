module CodeGenerator where

import           Control.Monad.State
import qualified Data.Map.Strict     as Map
import           GoatAST
import           GoatExit
import           GoatPrettyPrint
import           SymbolTable
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
                               ; let procedures = Map.keys programMap
                               ; generateProcedureList procedures programMap
                               }

generateProcedureList :: [String] -> ProgramMap -> IO ()
generateProcedureList (procedure:[]) programMap =
    case Map.lookup procedure programMap of
        Just procedureTable -> do { putStrLn $ "proc_" ++ procedure ++ ":"
                                  ; generateProcedure procedure procedureTable
                                  ; printNewLineIndentation
                                  ; putStrLn "return"
                                  }

generateProcedureList (procedure:procedures) programMap =
    case Map.lookup procedure programMap of
        Just procedureTable -> do { putStrLn $ procedure ++ ":"
                                  ; generateProcedure procedure procedureTable
                                  ; printNewLineIndentation
                                  ; putStrLn "return"
                                  ; generateProcedureList procedures programMap
                                  }

generateProcedure :: Identifier -> ProcedureTable -> IO ()
generateProcedure procName (ProcedureTable paramMap varMap statements) = do
    let parameterNumber = Map.size paramMap
        variableNumber = Map.size varMap
        totalVarNumber = parameterNumber + variableNumber
    case totalVarNumber of
        0 -> putStr ""
        otherwise -> printLine $ "push_stack_frame " ++ (show totalVarNumber)
    let stackMap = insertStackMap paramMap varMap
    generateStatements procName [0] statements stackMap
    case totalVarNumber of
        0 -> putStr ""
        otherwise -> printLine $ "pop_stack_frame " ++ (show totalVarNumber)




generateStatements :: String -> [Int] -> [StatementTable] -> StackMap -> IO ()
generateStatements _ _ [] _  = return ()
generateStatements procName label (stat:[]) stackMap = do
    generateStatement procName (updateLabel label) stat stackMap
generateStatements procName label (stat:stats) stackMap = do
    { generateStatement procName (updateLabel label) stat stackMap
    ; generateStatements procName (updateLabel label) stats stackMap
    }

generateStatement :: String -> [Int] -> StatementTable -> StackMap -> IO ()
generateStatement procName label statementTable stackMap = do
  case statementTable of
    WriteTable exprTable -> do { generateWriteStatement exprTable }
    IfTable exprTable stmtTables ->
      generateIfStatement procName label exprTable stmtTables stackMap
    IfElseTable exprTable stmtTables1 stmtTables2 ->
      generateIfElseStatement procName label exprTable stmtTables1 stmtTables2 stackMap
    WhileTable exprTable stmtTables ->
      generateWhileStatement procName label exprTable stmtTables stackMap
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
           -- Expression
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
        BoolTable val -> printLine ("int_const r" ++ (show registerNum) ++
                                    ", " ++ (convertBoolToInt val))
        IntTable val -> do { printNewLineIndentation
                           ; putStrLn $ "int_const r" ++ (show registerNum)
                             ++ ", " ++ (show val)
                           }
        FloatTable val -> do { printNewLineIndentation
                             ; putStrLn $ "real_const r" ++ (show registerNum)
                               ++ ", " ++ (show val)
                             }
        StringTable val -> printLine ("string_const r" ++ (show registerNum) ++
                                      ", " ++ val)
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
        OrTable    lExpr rExpr _        -> generateOrExpression    lExpr rExpr registerNum
        AndTable   lExpr rExpr _        -> generateAndExpression   lExpr rExpr registerNum
        EqTable    lExpr rExpr exprType -> generateEqExpression    lExpr rExpr registerNum exprType
        NotEqTable lExpr rExpr exprType -> generateNotEqExpression lExpr rExpr registerNum exprType
        LesTable   lExpr rExpr exprType -> generateLesExpression   lExpr rExpr registerNum exprType
        LesEqTable lExpr rExpr exprType -> generateLesEqExpression lExpr rExpr registerNum exprType
        GrtTable   lExpr rExpr exprType -> generateGrtExpression   lExpr rExpr registerNum exprType
        GrtEqTable lExpr rExpr exprType -> generateGrtEqExpression lExpr rExpr registerNum exprType
        NotTable   expr        _ -> generateNotExpression   expr        registerNum
        -- TODO Unary Minus







-- generateReadStatement :: ExpressionTable -> IO ()
-- generateReadStatement exprTable = do {}


updateLabel :: [Int] -> [Int]
updateLabel (x:[]) = (x+1):[]
updateLabel (x:xs) = x:(updateLabel xs)

showLabel :: [Int] -> String
showLabel (x:[]) = show(x)
showLabel (x:xs) = show(x) ++ "_" ++ showLabel(xs)

generateIfStatement ::
  String -> [Int] -> ExpressionTable -> [StatementTable] -> StackMap -> IO ()
generateIfStatement procName label exprTable stmts stackMap = do
  { let label_a = procName ++ "_" ++ (showLabel label) ++ "_a"
  ; let label_b = procName ++ "_" ++ (showLabel label) ++ "_b"
  -- check condition
  ; generateExpression exprTable 0
  ; printLine ("branch_on_true r0, " ++ label_a)
  ; printLine ("branch_uncond " ++ label_b)
  -- If statements
  ; putStrLn (label_a ++ ":")
  ; generateStatements procName (label ++ [0] ++ [0]) stmts stackMap
  -- end of this statements
  ; putStrLn (label_b ++ ":")
  }

generateIfElseStatement ::
  String -> [Int] -> ExpressionTable -> [StatementTable] -> [StatementTable]
  -> StackMap -> IO ()
generateIfElseStatement procName label exprTable stmts1 stmts2 stackMap = do
  { let label_a = procName ++ "_" ++ (showLabel label) ++ "_a"
  ; let label_b = procName ++ "_" ++ (showLabel label) ++ "_b"
  ; generateExpression exprTable 0
  -- Else statements
  ; printLine ("branch_on_false r0, " ++ label_a)
  -- If statements
  ; generateStatements procName (label ++ [1] ++ [0]) stmts1 stackMap
  ; printLine ("branch_uncond " ++ label_b)
  -- Else statements
  ; putStrLn (label_a ++ ":")
  ; generateStatements procName (label ++ [2] ++ [0]) stmts2 stackMap
  -- fi The end of If-Else
  ; putStrLn (label_b ++ ":")
  }

generateWhileStatement ::
  String -> [Int] -> ExpressionTable -> [StatementTable] -> StackMap -> IO ()
generateWhileStatement procName label exprTable stmts stackMap = do
  { let label_a = procName ++ "_" ++ (showLabel label) ++ "_a"
  ; let label_b = procName ++ "_" ++ (showLabel label) ++ "_b"
  ; let label_c = procName ++ "_" ++ (showLabel label) ++ "_c"
  -- check condition
  ; putStrLn (label_a ++ ":")
  ; generateExpression exprTable 0
  ; printLine ("branch_on_true r0, " ++ label_b)
  ; printLine ("branch_uncond " ++ label_c)
  -- while statements
  ; putStrLn (label_b ++ ":")
  ; generateStatements procName (label ++ [3] ++ [0]) stmts stackMap
  -- check condition again
  ; printLine ("branch_uncond " ++ label_a)
  -- end of this while loop
  ; putStrLn (label_c ++ ":")
  }

generateOrExpression :: ExpressionTable -> ExpressionTable -> Int -> IO ()
generateOrExpression lExpr rExpr regNum = do
  generateAndOrExpr "or" lExpr rExpr regNum

generateAndExpression :: ExpressionTable -> ExpressionTable -> Int -> IO ()
generateAndExpression lExpr rExpr regNum = do
  generateAndOrExpr "and" lExpr rExpr regNum

generateAndOrExpr ::
  String -> ExpressionTable -> ExpressionTable -> Int -> IO ()
generateAndOrExpr operator lExpr rExpr regNum = do
  { generateExpression lExpr regNum
  ; generateExpression rExpr (regNum+1)
  ; printLine $ operator ++ " r" ++ (show regNum) ++ ", r" ++ (show regNum) ++
                ", r" ++ (show (regNum+1))
  }


generateNotExpression :: ExpressionTable -> Int -> IO ()
generateNotExpression expr regNum = do
  { generateExpression expr regNum
  ; printLine $ "not r" ++ (show regNum) ++ ", r" ++ (show regNum)
  }

generateEqExpression ::
  ExpressionTable -> ExpressionTable -> Int -> BaseType -> IO ()
generateEqExpression lExpr rExpr regNum exprType = do
  generateCompareExpr "cmp_eq" lExpr rExpr regNum exprType

generateNotEqExpression ::
  ExpressionTable -> ExpressionTable -> Int -> BaseType -> IO ()
generateNotEqExpression lExpr rExpr regNum exprType = do
  generateCompareExpr "cmp_ne" lExpr rExpr regNum exprType

generateLesExpression ::
  ExpressionTable -> ExpressionTable -> Int -> BaseType -> IO ()
generateLesExpression lExpr rExpr regNum exprType = do
  generateCompareExpr "cmp_lt" lExpr rExpr regNum exprType

generateLesEqExpression ::
  ExpressionTable -> ExpressionTable -> Int -> BaseType -> IO ()
generateLesEqExpression lExpr rExpr regNum exprType = do
  generateCompareExpr "cmp_le" lExpr rExpr regNum exprType

generateGrtExpression ::
  ExpressionTable -> ExpressionTable -> Int -> BaseType -> IO ()
generateGrtExpression lExpr rExpr regNum exprType = do
  generateCompareExpr "cmp_gt" lExpr rExpr regNum exprType

generateGrtEqExpression ::
  ExpressionTable -> ExpressionTable -> Int -> BaseType -> IO ()
generateGrtEqExpression lExpr rExpr regNum exprType = do
  generateCompareExpr "cmp_ge" lExpr rExpr regNum exprType

generateCompareExpr ::
  String -> ExpressionTable -> ExpressionTable -> Int -> BaseType -> IO ()
generateCompareExpr operator lExpr rExpr regNum exprType = do
  { generateExpression lExpr regNum
  ; generateExpression rExpr (regNum+1)
  ; case exprType of
      FloatType -> do
        generateIntToFloat lExpr rExpr regNum
        printLine $ operator ++ "_real r" ++ (show regNum) ++ ", r" ++
                    (show regNum) ++ ", r" ++ (show (regNum+1))
      otherwise ->
        printLine $ operator ++ "_int r" ++ (show regNum) ++ ", r" ++
                    (show regNum) ++ ", r" ++ (show (regNum+1))
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
          IntTable _            -> IntType
          FloatTable _          -> FloatType
          BoolTable _           -> BoolType
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

insertStackMap :: ParameterMap -> VariableMap -> StackMap
insertStackMap paramMap varMap = do
    let paramList = Map.keys paramMap
        varList = Map.keys varMap
        stackList = paramList ++ varList
    subinsertStackMap stackList 0


subinsertStackMap :: [String] -> Int -> StackMap
subinsertStackMap (name:[]) index = Map.insert name index Map.empty
subinsertStackMap (name:names) index = Map.insert name index (subinsertStackMap names $ index+1)
