module CodeGenerator where

import           Control.Monad.State
import qualified Data.Map.Strict     as Map
import           GoatAST
import           GoatExit
import           GoatPrettyPrint
import           SymbolTable
import           Util
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
codeGeneration programMap = do { printLine "call proc_main"
                               ; printLine "halt"
                               ; let procedures = Map.keys programMap
                               ; generateProcedureList procedures programMap
                               }

generateProcedureList :: [String] -> ProgramMap -> IO ()
generateProcedureList (procedure:[]) programMap =
    case Map.lookup procedure programMap of
        Just procedureTable -> do { putStrLn $ "proc_" ++ procedure ++ ":"
                                  ; generateProcedure procedure procedureTable
                                  ; printLine "return"
                                  }
generateProcedureList (procedure:procedures) programMap =
    case Map.lookup procedure programMap of
        Just procedureTable -> do { putStrLn $ "proc_" ++ procedure ++ ":"
                                  ; generateProcedure procedure procedureTable
                                  ; printLine "return"
                                  ; generateProcedureList procedures programMap
                                  }

generateProcedure :: Identifier -> ProcedureTable -> IO ()
generateProcedure procName (ProcedureTable paramMap varMap statements) = do
    let parameterNumber = Map.size paramMap
        variableNumber  = Map.size varMap
        totalVarNumber  = parameterNumber + variableNumber
    case totalVarNumber of
        0         -> putStr ""
        otherwise -> printLine $ "push_stack_frame " ++ (show totalVarNumber)
    let stackMap = insertStackMap paramMap varMap
        varList = Map.keys varMap
    -- TODO
    -- initParameters
    case variableNumber of
        0         -> putStr ""
        otherwise -> do { printLine "int_const r0, 0"
                        -- TODO
                        -- initArrayMatrix
                        ; initVariables varList stackMap
                        }
    generateStatements procName [0] statements stackMap
    case totalVarNumber of
        0         -> putStr ""
        otherwise -> printLine $ "pop_stack_frame " ++ (show totalVarNumber)

initVariables :: [Identifier] -> StackMap -> IO ()
initVariables [] _ = return ()
initVariables (var:[]) stackMap = do
  let varSlotNum = stackMap Map.! var
  printLine $ "store " ++ (show varSlotNum) ++ ", r0"
initVariables (var:varList) stackMap = do
  let varSlotNum = stackMap Map.! var
  printLine $ "store " ++ (show varSlotNum) ++ ", r0"
  initVariables varList stackMap

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
    AssignTable varTable  exprTable  ->
      generateAssignStatement procName varTable exprTable stackMap
    WriteTable  exprTable            -> generateWriteStatement exprTable stackMap
    IfTable     exprTable stmtTables ->
      generateIfStatement procName label exprTable stmtTables stackMap
    IfElseTable exprTable stmtTables1 stmtTables2 ->
      generateIfElseStatement procName label exprTable stmtTables1 stmtTables2 stackMap
    WhileTable  exprTable stmtTables ->
      generateWhileStatement procName label exprTable stmtTables stackMap
    -- TODO
    -- ReadTable
    -- CallTable

generateAssignStatement ::
  String -> ExpressionTable -> ExpressionTable -> StackMap -> IO ()
generateAssignStatement procName varTable exprTable stackMap = do
  -- TODO Array Matrix
  { let slotNum = getVariableSlotNum (variable varTable) stackMap
        varType = variableType varTable
        exprType = getAssignBaseType exprTable
  ; generateExpression exprTable 0 stackMap
  ; if (FloatType == varType) && (IntType == exprType)
        then printIntToRealInSameRegister 0
        else putStr ""
  ; printLine $ "store " ++ (show slotNum) ++ ", r0"
  }

generateWriteStatement :: ExpressionTable -> StackMap -> IO ()
generateWriteStatement exprTable stackMap =
    case exprTable of
        VariableTable _ exprType ->
            generateWriteChooseType exprType exprTable stackMap
        BoolTable   _ -> generateWriteWithType "int"    exprTable stackMap
        IntTable    _ -> generateWriteWithType "int"    exprTable stackMap
        FloatTable  _ -> generateWriteWithType "real"   exprTable stackMap
        StringTable _ -> generateWriteWithType "string" exprTable stackMap
        AddTable    _ _ exprType ->
            generateWriteChooseType exprType exprTable stackMap
        SubTable    _ _ exprType ->
            generateWriteChooseType exprType exprTable stackMap
        MulTable    _ _ exprType ->
            generateWriteChooseType exprType exprTable stackMap
        DivTable    _ _ exprType ->
            generateWriteChooseType exprType exprTable stackMap
        NegativeTable _ exprType ->
            generateWriteChooseType exprType exprTable stackMap
        otherwise -> generateWriteWithType "int" exprTable stackMap

generateWriteWithType :: String -> ExpressionTable -> StackMap -> IO ()
generateWriteWithType writeType exprTable stackMap = do
  { generateExpression exprTable 0 stackMap
  ; printLine $ "call_builtin print_" ++ writeType
  }

generateWriteChooseType :: BaseType -> ExpressionTable -> StackMap -> IO ()
generateWriteChooseType exprType exprTable stackMap =
  case exprType of
    FloatType -> generateWriteWithType "real" exprTable stackMap
    otherwise -> generateWriteWithType "int"  exprTable stackMap

-- generateReadStatement :: ExpressionTable -> IO ()
-- generateReadStatement exprTable = do {}

generateExpression :: ExpressionTable -> Int -> StackMap -> IO ()
generateExpression exprTable registerNum stackMap =
    case exprTable of
        VariableTable var varType ->
          generateVariableExpr var varType registerNum stackMap
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
                                      ", " ++ "\"" ++ val ++ "\"")
        AddTable lExpr rExpr baseType -> do
              generateExpression lExpr registerNum stackMap
              generateExpression rExpr (registerNum+1) stackMap
              case baseType of
                   IntType -> do { generateOperationString "add" "int" registerNum }
                   FloatType -> do { generateIntToFloat lExpr rExpr registerNum
                                   ; generateOperationString "add" "real" registerNum
                                   }
        SubTable lExpr rExpr baseType -> do
              generateExpression lExpr registerNum stackMap
              generateExpression rExpr (registerNum+1) stackMap
              case baseType of
                   IntType -> do { generateOperationString "sub" "int" registerNum }
                   FloatType -> do { generateIntToFloat lExpr rExpr registerNum
                                   ; generateOperationString "sub" "real" registerNum
                                   }
        MulTable lExpr rExpr baseType -> do
              generateExpression lExpr registerNum stackMap
              generateExpression rExpr (registerNum+1) stackMap
              case baseType of
                   IntType -> do { generateOperationString "mul" "int" registerNum }
                   FloatType -> do { generateIntToFloat lExpr rExpr registerNum
                                   ; generateOperationString "mul" "real" registerNum
                                   }
        DivTable lExpr rExpr baseType -> do
              generateExpression lExpr registerNum stackMap
              generateExpression rExpr (registerNum+1) stackMap
              case baseType of
                   IntType -> do { generateOperationString "div" "int" registerNum }
                   FloatType -> do { generateIntToFloat lExpr rExpr registerNum
                                   ; generateOperationString "div" "real" registerNum
                                   }
        OrTable    lExpr rExpr _        -> generateOrExpression    lExpr rExpr registerNum stackMap
        AndTable   lExpr rExpr _        -> generateAndExpression   lExpr rExpr registerNum stackMap
        EqTable    lExpr rExpr exprType -> generateEqExpression    lExpr rExpr registerNum exprType stackMap
        NotEqTable lExpr rExpr exprType -> generateNotEqExpression lExpr rExpr registerNum exprType stackMap
        LesTable   lExpr rExpr exprType -> generateLesExpression   lExpr rExpr registerNum exprType stackMap
        LesEqTable lExpr rExpr exprType -> generateLesEqExpression lExpr rExpr registerNum exprType stackMap
        GrtTable   lExpr rExpr exprType -> generateGrtExpression   lExpr rExpr registerNum exprType stackMap
        GrtEqTable lExpr rExpr exprType -> generateGrtEqExpression lExpr rExpr registerNum exprType stackMap
        NegativeTable     expr exprType -> generateNegativeExpression expr registerNum exprType stackMap
        NotTable   expr  _ -> generateNotExpression expr registerNum stackMap

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
  ; generateExpression exprTable 0 stackMap
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
  ; generateExpression exprTable 0 stackMap
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
  ; generateExpression exprTable 0 stackMap
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

generateVariableExpr :: Variable -> BaseType -> Int -> StackMap -> IO ()
generateVariableExpr var varType regNum stackMap = do
  { let varShape = varShapeIndicator var
        varSlotNum = getVariableSlotNum var stackMap
  ; case varShape of
      NoIndicator ->
        printLine $ "load r" ++ (show regNum) ++ ", " ++ (show varSlotNum)
      -- TODO Array Matrix
      Array  n    -> return ()
      Matrix m n  -> return ()

  }

generateOrExpression ::
  ExpressionTable -> ExpressionTable -> Int -> StackMap -> IO ()
generateOrExpression lExpr rExpr regNum stackMap = do
  generateAndOrExpr "or" lExpr rExpr regNum stackMap

generateAndExpression ::
  ExpressionTable -> ExpressionTable -> Int -> StackMap -> IO ()
generateAndExpression lExpr rExpr regNum stackMap = do
  generateAndOrExpr "and" lExpr rExpr regNum stackMap

generateAndOrExpr ::
  String -> ExpressionTable -> ExpressionTable -> Int -> StackMap -> IO ()
generateAndOrExpr operator lExpr rExpr regNum stackMap = do
  { generateExpression lExpr regNum stackMap
  ; generateExpression rExpr (regNum+1) stackMap
  ; printLine $ operator ++ " r" ++ (show regNum) ++ ", r" ++ (show regNum) ++
                ", r" ++ (show (regNum+1))
  }

generateNotExpression :: ExpressionTable -> Int -> StackMap -> IO ()
generateNotExpression expr regNum stackMap = do
  { generateExpression expr regNum stackMap
  ; printLine $ "not r" ++ (show regNum) ++ ", r" ++ (show regNum)
  }

generateEqExpression ::
  ExpressionTable -> ExpressionTable -> Int -> BaseType -> StackMap -> IO ()
generateEqExpression lExpr rExpr regNum exprType stackMap = do
  generateCompareExpr "cmp_eq" lExpr rExpr regNum exprType stackMap

generateNotEqExpression ::
  ExpressionTable -> ExpressionTable -> Int -> BaseType -> StackMap -> IO ()
generateNotEqExpression lExpr rExpr regNum exprType stackMap = do
  generateCompareExpr "cmp_ne" lExpr rExpr regNum exprType stackMap

generateLesExpression ::
  ExpressionTable -> ExpressionTable -> Int -> BaseType -> StackMap -> IO ()
generateLesExpression lExpr rExpr regNum exprType stackMap = do
  generateCompareExpr "cmp_lt" lExpr rExpr regNum exprType stackMap

generateLesEqExpression ::
  ExpressionTable -> ExpressionTable -> Int -> BaseType -> StackMap -> IO ()
generateLesEqExpression lExpr rExpr regNum exprType stackMap = do
  generateCompareExpr "cmp_le" lExpr rExpr regNum exprType stackMap

generateGrtExpression ::
  ExpressionTable -> ExpressionTable -> Int -> BaseType -> StackMap -> IO ()
generateGrtExpression lExpr rExpr regNum exprType stackMap = do
  generateCompareExpr "cmp_gt" lExpr rExpr regNum exprType stackMap

generateGrtEqExpression ::
  ExpressionTable -> ExpressionTable -> Int -> BaseType -> StackMap -> IO ()
generateGrtEqExpression lExpr rExpr regNum exprType stackMap = do
  generateCompareExpr "cmp_ge" lExpr rExpr regNum exprType stackMap

generateCompareExpr ::
  String -> ExpressionTable -> ExpressionTable -> Int -> BaseType -> StackMap -> IO ()
generateCompareExpr operator lExpr rExpr regNum exprType stackMap = do
  { generateExpression lExpr regNum stackMap
  ; generateExpression rExpr (regNum+1) stackMap
  ; case exprType of
      FloatType -> do
        generateIntToFloat lExpr rExpr regNum
        printLine $ operator ++ "_real r" ++ (show regNum) ++ ", r" ++
                    (show regNum) ++ ", r" ++ (show (regNum+1))
      otherwise ->
        printLine $ operator ++ "_int r" ++ (show regNum) ++ ", r" ++
                    (show regNum) ++ ", r" ++ (show (regNum+1))
  }

generateNegativeExpression ::
  ExpressionTable -> Int -> BaseType -> StackMap -> IO ()
generateNegativeExpression expr registerNum exprType stackMap = do
  { let regNum = show registerNum
  ; generateExpression expr registerNum stackMap
  ; case exprType of
      IntType   -> printLine $ "neg_int r"  ++ regNum ++ ", r" ++ regNum
      FloatType -> printLine $ "neg_real r" ++ regNum ++ ", r" ++ regNum
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





------------------------------- Helper functions ------------------------------

printNewLineIndentation :: IO ()
printNewLineIndentation = putStr "    "

-------------------------------------------------------------------------------
-- Create the Oz statement: int_to_real r0 r1, where 0 and 1 are provided
-- register numbers.
-------------------------------------------------------------------------------
printIntToRealInNewRegister :: Int -> Int -> IO ()
printIntToRealInNewRegister targetRegisterNumber sourceRegisterNumber = do
  printLine $ "int_to_real r" ++ (show targetRegisterNumber) ++
              ", r" ++ (show sourceRegisterNumber)

-------------------------------------------------------------------------------
-- Create the Oz statement: int_to_real r0 r0, where 0 is the provided register
-- number.
-------------------------------------------------------------------------------
printIntToRealInSameRegister :: Int -> IO ()
printIntToRealInSameRegister registerNumber = do
  printIntToRealInNewRegister registerNumber registerNumber

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
        (IntType,FloatType) -> printIntToRealInSameRegister registerNum
        (FloatType,IntType) -> printIntToRealInSameRegister (registerNum + 1)

getVariableSlotNum :: Variable -> StackMap -> Int
getVariableSlotNum variable stackMap = stackMap Map.! varName
  where varName = (varId variable)


insertStackMap :: ParameterMap -> VariableMap -> StackMap
insertStackMap paramMap varMap = do
    let paramList = Map.keys paramMap
        varList = Map.keys varMap
        stackList = paramList ++ varList
    subinsertStackMap stackList 0


subinsertStackMap :: [String] -> Int -> StackMap
subinsertStackMap (name:[]) index = Map.insert name index Map.empty
subinsertStackMap (name:names) index = Map.insert name index (subinsertStackMap names $ index+1)
