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
        varList         = Map.keys varMap
        variableNumber  = getVariableMapSize varList varMap
        totalVarNumber  = parameterNumber + variableNumber
    case totalVarNumber of
        0         -> putStr ""
        otherwise -> printLine $ "push_stack_frame " ++ (show totalVarNumber)
    let stackMap = insertStackMap paramMap varMap
    -- TODO
    -- initParameters
    case variableNumber of
        0         -> putStr ""
        otherwise -> do { printComment "init variables"
                        ; printLine "int_const r0, 0"
                        ; initVariables varList varMap stackMap
                        }
    generateStatements procName [0] varMap statements stackMap
    case totalVarNumber of
        0         -> putStr ""
        otherwise -> printLine $ "pop_stack_frame " ++ (show totalVarNumber)

initVariables :: [Identifier] -> VariableMap -> StackMap -> IO ()
initVariables [] _ _ = return ()
initVariables (var:[]) varMap stackMap = do
  let varSlotNum   = stackMap Map.! var
      varIndicator = varShapeIndicator $ declarationVariable (varMap Map.! var)
  printComment $ "initialise variable " ++ var
  initVariableWithIndicator varIndicator varSlotNum
initVariables (var:varList) varMap stackMap = do
  let varSlotNum = stackMap Map.! var
      varIndicator = varShapeIndicator $ declarationVariable (varMap Map.! var)
  printComment $ "init variable: " ++ var
  initVariableWithIndicator varIndicator varSlotNum
  initVariables varList varMap stackMap

initVariableWithIndicator :: ShapeIndicator -> Int -> IO ()
initVariableWithIndicator varIndicator varSlotNum = do
  case varIndicator of
    NoIndicator                       -> initSingleVar varSlotNum
    Array  (IntConst n)               -> initOffset    varSlotNum n
    Matrix (IntConst m) (IntConst n)  -> initOffset    varSlotNum (m*n)

initSingleVar :: Int -> IO ()
initSingleVar varSlotNum = printLine $ "store " ++ (show varSlotNum) ++ ", r0"

initOffset :: Int -> Int -> IO ()
initOffset varSlotNum offset = do
  if offset > 0
    then do { initSingleVar varSlotNum
            ; initOffset (varSlotNum+1) (offset-1)
            }
    else putStr ""

generateStatements :: String -> [Int] -> VariableMap -> [StatementTable] -> StackMap -> IO ()
generateStatements _ _ _ [] _  = return ()
generateStatements procName label varMap (stat:[]) stackMap = do
    generateStatement procName (updateLabel label) varMap stat stackMap
generateStatements procName label varMap (stat:stats) stackMap = do
    { generateStatement procName (updateLabel label) varMap stat stackMap
    ; generateStatements procName (updateLabel label) varMap stats stackMap
    }

generateStatement :: String -> [Int] -> VariableMap -> StatementTable -> StackMap -> IO ()
generateStatement procName label varMap statementTable stackMap = do
  case statementTable of
    AssignTable varTable  exprTable  ->
      generateAssignStatement procName varMap varTable exprTable stackMap
    WriteTable  exprTable            -> generateWriteStatement varMap exprTable stackMap
    IfTable     exprTable stmtTables ->
      generateIfStatement procName label exprTable varMap stmtTables stackMap
    IfElseTable exprTable stmtTables1 stmtTables2 ->
      generateIfElseStatement procName label exprTable varMap stmtTables1 stmtTables2 stackMap
    WhileTable  exprTable stmtTables ->
      generateWhileStatement procName label exprTable varMap stmtTables stackMap
    -- TODO
    ReadTable exprTable ->
      generateReadStatement exprTable stackMap
    -- CallTable

generateAssignStatement ::
  String -> VariableMap -> ExpressionTable -> ExpressionTable -> StackMap -> IO ()
generateAssignStatement procName varMap varTable exprTable stackMap = do
  { let var     = variable varTable
        varType = variableType varTable
        varId   = varName  var
        exprType = getAssignBaseType exprTable
        varShape = varShapeIndicatorTable var
        varSlotNum = getVariableSlotNum var stackMap
        varSlotNumStr = show varSlotNum
  ; printComment $ "Assign statement for variable " ++ varId
  -- Right expression
  ; generateExpression varMap exprTable 0 stackMap
  ; if (FloatType == varType) && (IntType == exprType)
        then printIntToRealInSameRegister 0
        else putStr ""
  -- Left Variable
  ; case varShape of
      NoIndicatorTable ->
          printLine $ "store " ++ varSlotNumStr ++ ", r0"
      otherwise        -> do
        { locateArrayMatrix varMap var varSlotNumStr 1 stackMap
        ; printLine $ "store_indirect r1, r0"
        }
  }

generateWriteStatement :: VariableMap -> ExpressionTable -> StackMap -> IO ()
generateWriteStatement varMap exprTable stackMap =
    case exprTable of
        VariableTable _ exprType ->
            generateWriteChooseType exprType varMap exprTable stackMap
        BoolTable   _ -> generateWriteWithType "int"    varMap exprTable stackMap
        IntTable    _ -> generateWriteWithType "int"    varMap exprTable stackMap
        FloatTable  _ -> generateWriteWithType "real"   varMap exprTable stackMap
        StringTable _ -> generateWriteWithType "string" varMap exprTable stackMap
        AddTable    _ _ exprType ->
            generateWriteChooseType exprType varMap exprTable stackMap
        SubTable    _ _ exprType ->
            generateWriteChooseType exprType varMap exprTable stackMap
        MulTable    _ _ exprType ->
            generateWriteChooseType exprType varMap exprTable stackMap
        DivTable    _ _ exprType ->
            generateWriteChooseType exprType varMap exprTable stackMap
        NegativeTable _ exprType ->
            generateWriteChooseType exprType varMap exprTable stackMap
        otherwise -> generateWriteWithType "int" varMap exprTable stackMap

generateWriteWithType :: String -> VariableMap -> ExpressionTable -> StackMap -> IO ()
generateWriteWithType writeType varMap exprTable stackMap = do
  { generateExpression varMap exprTable 0 stackMap
  ; printLine $ "call_builtin print_" ++ writeType
  }

generateWriteChooseType :: BaseType -> VariableMap -> ExpressionTable -> StackMap -> IO ()
generateWriteChooseType exprType varMap exprTable stackMap =
  case exprType of
    FloatType -> generateWriteWithType "real" varMap exprTable stackMap
    otherwise -> generateWriteWithType "int"  varMap exprTable stackMap

generateReadStatement :: ExpressionTable -> StackMap -> IO ()
generateReadStatement exprTable stackMap = do
    let exprType = getExprType exprTable
        slotNum = stackMap Map.! (varName $ variable exprTable)
    case exprType of
        BoolType  -> generateReadStatementByType "bool" slotNum
        IntType   -> generateReadStatementByType "int" slotNum
        FloatType -> generateReadStatementByType "real" slotNum

generateReadStatementByType :: String -> Int -> IO ()
generateReadStatementByType baseType slotNum = do
    printLine ("call_builtin read_" ++ baseType)
    printLine ("store " ++ (show slotNum) ++ ", r0")


generateExpression :: VariableMap -> ExpressionTable -> Int -> StackMap -> IO ()
generateExpression varMap exprTable registerNum stackMap =
    case exprTable of
        VariableTable var varType ->
          generateVariableExpr varMap var varType registerNum stackMap
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
              generateExpression varMap lExpr registerNum stackMap
              generateExpression varMap rExpr (registerNum+1) stackMap
              case baseType of
                   IntType -> do { generateOperationString "add" "int" registerNum }
                   FloatType -> do { generateIntToFloat lExpr rExpr registerNum
                                   ; generateOperationString "add" "real" registerNum
                                   }
        SubTable lExpr rExpr baseType -> do
              generateExpression varMap lExpr registerNum stackMap
              generateExpression varMap rExpr (registerNum+1) stackMap
              case baseType of
                   IntType -> do { generateOperationString "sub" "int" registerNum }
                   FloatType -> do { generateIntToFloat lExpr rExpr registerNum
                                   ; generateOperationString "sub" "real" registerNum
                                   }
        MulTable lExpr rExpr baseType -> do
              generateExpression varMap lExpr registerNum stackMap
              generateExpression varMap rExpr (registerNum+1) stackMap
              case baseType of
                   IntType -> do { generateOperationString "mul" "int" registerNum }
                   FloatType -> do { generateIntToFloat lExpr rExpr registerNum
                                   ; generateOperationString "mul" "real" registerNum
                                   }
        DivTable lExpr rExpr baseType -> do
              generateExpression varMap lExpr registerNum stackMap
              generateExpression varMap rExpr (registerNum+1) stackMap
              case baseType of
                   IntType -> do { generateOperationString "div" "int" registerNum }
                   FloatType -> do { generateIntToFloat lExpr rExpr registerNum
                                   ; generateOperationString "div" "real" registerNum
                                   }
        OrTable    lExpr rExpr _        -> generateOrExpression    varMap lExpr rExpr registerNum stackMap
        AndTable   lExpr rExpr _        -> generateAndExpression   varMap lExpr rExpr registerNum stackMap
        EqTable    lExpr rExpr exprType -> generateEqExpression    varMap lExpr rExpr registerNum exprType stackMap
        NotEqTable lExpr rExpr exprType -> generateNotEqExpression varMap lExpr rExpr registerNum exprType stackMap
        LesTable   lExpr rExpr exprType -> generateLesExpression   varMap lExpr rExpr registerNum exprType stackMap
        LesEqTable lExpr rExpr exprType -> generateLesEqExpression varMap lExpr rExpr registerNum exprType stackMap
        GrtTable   lExpr rExpr exprType -> generateGrtExpression   varMap lExpr rExpr registerNum exprType stackMap
        GrtEqTable lExpr rExpr exprType -> generateGrtEqExpression varMap lExpr rExpr registerNum exprType stackMap
        NegativeTable     expr exprType -> generateNegativeExpression varMap expr registerNum exprType stackMap
        NotTable   expr  _ -> generateNotExpression varMap expr registerNum stackMap

updateLabel :: [Int] -> [Int]
updateLabel (x:[]) = (x+1):[]
updateLabel (x:xs) = x:(updateLabel xs)

showLabel :: [Int] -> String
showLabel (x:[]) = show(x)
showLabel (x:xs) = show(x) ++ "_" ++ showLabel(xs)

generateIfStatement ::
  String -> [Int] -> ExpressionTable -> VariableMap -> [StatementTable] -> StackMap -> IO ()
generateIfStatement procName label exprTable varMap stmts stackMap = do
  { let label_a = procName ++ "_" ++ (showLabel label) ++ "_a"
  ; let label_b = procName ++ "_" ++ (showLabel label) ++ "_b"
  -- check condition
  ; generateExpression varMap exprTable 0 stackMap
  ; printLine ("branch_on_true r0, " ++ label_a)
  ; printLine ("branch_uncond " ++ label_b)
  -- If statements
  ; putStrLn (label_a ++ ":")
  ; generateStatements procName (label ++ [0] ++ [0]) varMap stmts stackMap
  -- end of this statements
  ; putStrLn (label_b ++ ":")
  }

generateIfElseStatement ::
  String -> [Int] -> ExpressionTable -> VariableMap -> [StatementTable] -> [StatementTable]
  -> StackMap -> IO ()
generateIfElseStatement procName label exprTable varMap stmts1 stmts2 stackMap = do
  { let label_a = procName ++ "_" ++ (showLabel label) ++ "_a"
  ; let label_b = procName ++ "_" ++ (showLabel label) ++ "_b"
  ; generateExpression varMap exprTable 0 stackMap
  -- Else statements
  ; printLine ("branch_on_false r0, " ++ label_a)
  -- If statements
  ; generateStatements procName (label ++ [1] ++ [0]) varMap stmts1 stackMap
  ; printLine ("branch_uncond " ++ label_b)
  -- Else statements
  ; putStrLn (label_a ++ ":")
  ; generateStatements procName (label ++ [2] ++ [0]) varMap stmts2 stackMap
  -- fi The end of If-Else
  ; putStrLn (label_b ++ ":")
  }

generateWhileStatement ::
  String -> [Int] -> ExpressionTable -> VariableMap -> [StatementTable] -> StackMap -> IO ()
generateWhileStatement procName label exprTable varMap stmts stackMap = do
  { let label_a = procName ++ "_" ++ (showLabel label) ++ "_a"
  ; let label_b = procName ++ "_" ++ (showLabel label) ++ "_b"
  ; let label_c = procName ++ "_" ++ (showLabel label) ++ "_c"
  -- check condition
  ; putStrLn (label_a ++ ":")
  ; generateExpression varMap exprTable 0 stackMap
  ; printLine ("branch_on_true r0, " ++ label_b)
  ; printLine ("branch_uncond " ++ label_c)
  -- while statements
  ; putStrLn (label_b ++ ":")
  ; generateStatements procName (label ++ [3] ++ [0]) varMap stmts stackMap
  -- check condition again
  ; printLine ("branch_uncond " ++ label_a)
  -- end of this while loop
  ; putStrLn (label_c ++ ":")
  }

generateVariableExpr :: VariableMap -> VariableSubTable -> BaseType -> Int -> StackMap -> IO ()
generateVariableExpr varMap var varType regNum stackMap = do
  { let varShape = varShapeIndicatorTable var
        varSlotNum = getVariableSlotNum var stackMap
        varSlotNumStr = show varSlotNum
        regNumStr0 = "r" ++ (show regNum)
  ; case varShape of
      NoIndicatorTable ->
          printLine $ "load " ++ regNumStr0 ++ ", " ++ varSlotNumStr
      otherwise        -> do
        { locateArrayMatrix varMap var varSlotNumStr regNum stackMap
        ; printLine $ "load_indirect " ++ regNumStr0 ++ ", " ++ regNumStr0
        }
  }

locateArrayMatrix ::
  VariableMap -> VariableSubTable -> String -> Int -> StackMap -> IO ()
locateArrayMatrix varMap var varSlotNumStr regNum stackMap = do
  let varShape = varShapeIndicatorTable var
      regNumStr0 = "r" ++ (show regNum)
      regNumStr1 = "r" ++ (show $ regNum + 1)
  case varShape of
    NoIndicatorTable -> putStr ""
    ArrayTable  expr -> do
      { printComment $ "Generate Array " ++ (varName var)
      ; generateExpression varMap expr regNum stackMap
      ; printLine $ "load_address " ++ regNumStr1 ++ ", " ++ varSlotNumStr
      ; printLine $ "sub_offset "   ++ regNumStr0 ++ ", " ++ regNumStr1 ++ ", " ++ regNumStr0
      }
    MatrixTable exprM exprN -> do
      { let varId    = varName var
            varDeclShape = varShapeIndicator $ declarationVariable (varMap Map.! varId)
            m = show $ getMatrixM varDeclShape
      ; printComment $ "Generate Matrix " ++ varId
      ; generateExpression varMap exprM regNum stackMap
      ; printLine $ "int_const " ++ regNumStr1 ++ ", " ++ m
      ; printLine $ "mul_int " ++ regNumStr0 ++ ", " ++ regNumStr0 ++ ", " ++ regNumStr1
      ; generateExpression varMap exprN (regNum+1) stackMap
      ; printLine $ "sub_int "  ++ regNumStr0 ++ ", " ++ regNumStr1 ++ ", " ++ regNumStr0
      ; printLine $ "load_address " ++ regNumStr1 ++ ", " ++ varSlotNumStr
      ; printLine $ "sub_offset "  ++ regNumStr0 ++ ", " ++ regNumStr1 ++ ", " ++ regNumStr0
      }


getMatrixM :: ShapeIndicator -> Int
getMatrixM (Matrix m _ ) =
  case m of
    IntConst n -> n

generateOrExpression ::
  VariableMap -> ExpressionTable -> ExpressionTable -> Int -> StackMap -> IO ()
generateOrExpression varMap lExpr rExpr regNum stackMap = do
  generateAndOrExpr "or" varMap lExpr rExpr regNum stackMap

generateAndExpression ::
  VariableMap -> ExpressionTable -> ExpressionTable -> Int -> StackMap -> IO ()
generateAndExpression varMap lExpr rExpr regNum stackMap = do
  generateAndOrExpr "and" varMap lExpr rExpr regNum stackMap

generateAndOrExpr ::
  String -> VariableMap -> ExpressionTable -> ExpressionTable -> Int -> StackMap -> IO ()
generateAndOrExpr operator varMap lExpr rExpr regNum stackMap = do
  { generateExpression varMap lExpr regNum stackMap
  ; generateExpression varMap rExpr (regNum+1) stackMap
  ; printLine $ operator ++ " r" ++ (show regNum) ++ ", r" ++ (show regNum) ++
                ", r" ++ (show (regNum+1))
  }

generateNotExpression :: VariableMap -> ExpressionTable -> Int -> StackMap -> IO ()
generateNotExpression varMap expr regNum stackMap = do
  { generateExpression varMap expr regNum stackMap
  ; printLine $ "not r" ++ (show regNum) ++ ", r" ++ (show regNum)
  }

generateEqExpression ::
  VariableMap -> ExpressionTable -> ExpressionTable -> Int -> BaseType -> StackMap -> IO ()
generateEqExpression varMap lExpr rExpr regNum exprType stackMap = do
  generateCompareExpr "cmp_eq" varMap lExpr rExpr regNum exprType stackMap

generateNotEqExpression ::
  VariableMap -> ExpressionTable -> ExpressionTable -> Int -> BaseType -> StackMap -> IO ()
generateNotEqExpression varMap lExpr rExpr regNum exprType stackMap = do
  generateCompareExpr "cmp_ne" varMap lExpr rExpr regNum exprType stackMap

generateLesExpression ::
  VariableMap -> ExpressionTable -> ExpressionTable -> Int -> BaseType -> StackMap -> IO ()
generateLesExpression varMap lExpr rExpr regNum exprType stackMap = do
  generateCompareExpr "cmp_lt" varMap lExpr rExpr regNum exprType stackMap

generateLesEqExpression ::
  VariableMap -> ExpressionTable -> ExpressionTable -> Int -> BaseType -> StackMap -> IO ()
generateLesEqExpression varMap lExpr rExpr regNum exprType stackMap = do
  generateCompareExpr "cmp_le" varMap lExpr rExpr regNum exprType stackMap

generateGrtExpression ::
  VariableMap -> ExpressionTable -> ExpressionTable -> Int -> BaseType -> StackMap -> IO ()
generateGrtExpression varMap lExpr rExpr regNum exprType stackMap = do
  generateCompareExpr "cmp_gt" varMap lExpr rExpr regNum exprType stackMap

generateGrtEqExpression ::
  VariableMap -> ExpressionTable -> ExpressionTable -> Int -> BaseType -> StackMap -> IO ()
generateGrtEqExpression varMap lExpr rExpr regNum exprType stackMap = do
  generateCompareExpr "cmp_ge" varMap lExpr rExpr regNum exprType stackMap

generateCompareExpr ::
  String -> VariableMap -> ExpressionTable -> ExpressionTable -> Int -> BaseType -> StackMap -> IO ()
generateCompareExpr operator varMap lExpr rExpr regNum exprType stackMap = do
  { generateExpression varMap lExpr regNum stackMap
  ; generateExpression varMap rExpr (regNum+1) stackMap
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
  VariableMap -> ExpressionTable -> Int -> BaseType -> StackMap -> IO ()
generateNegativeExpression varMap expr registerNum exprType stackMap = do
  { let regNum = show registerNum
  ; generateExpression varMap expr registerNum stackMap
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

printComment :: String -> IO ()
printComment string = do
  { putStr "  # "
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
          IntTable _               -> IntType
          FloatTable _             -> FloatType
          BoolTable _              -> BoolType
          VariableTable _ baseType -> baseType
          AddTable _ _ baseType    -> baseType
          SubTable _ _ baseType    -> baseType
          MulTable _ _ baseType    -> baseType
          DivTable _ _ baseType    -> baseType

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

getVariableSlotNum :: VariableSubTable -> StackMap -> Int
getVariableSlotNum variable stackMap = stackMap Map.! varId
  where varId = varName variable

getVariableMapSize :: [Identifier] -> VariableMap -> Int
getVariableMapSize [] _ = 0
getVariableMapSize (var:[]) varMap =
  getVariableSize varIndicator
  where varIndicator = varShapeIndicator $ declarationVariable (varMap Map.! var)
getVariableMapSize (var:vars) varMap =
  (getVariableSize varIndicator) + (getVariableMapSize vars varMap)
  where varIndicator = varShapeIndicator $ declarationVariable (varMap Map.! var)

getVariableSize :: ShapeIndicator -> Int
getVariableSize varIndicator =
  case varIndicator of
    NoIndicator                      -> 1
    Array  (IntConst n)              -> n
    Matrix (IntConst m) (IntConst n) -> m*n

insertStackMap :: ParameterMap -> VariableMap -> StackMap
insertStackMap paramMap varMap = do
    let paramList = Map.keys paramMap
        varList = Map.keys varMap
        stackList = paramList ++ varList
    subinsertStackMap stackList 0 varMap


-------------------------------------------------------------------------------
-- Example:
-- Using add_offset to access Array and Matrix
--   Array x[4]    from slot 0 to 4
--   Matrix x[2,3] from slot 0 to 6
-------------------------------------------------------------------------------
subinsertStackMap :: [String] -> Int -> VariableMap -> StackMap
subinsertStackMap (name:[]) index varMap =
  case (Map.member name varMap) of
    True  -> Map.insert name index Map.empty
        where varIndicator = varShapeIndicator $ declarationVariable (varMap Map.! name)
              varSize = getVariableSize varIndicator
    False -> Map.insert name index Map.empty
subinsertStackMap (name:names) index varMap =
  case (Map.member name varMap) of
    True  -> Map.insert name index (subinsertStackMap names (index+varSize) varMap)
        where varIndicator = varShapeIndicator $ declarationVariable (varMap Map.! name)
              varSize = getVariableSize varIndicator
    False -> Map.insert name index (subinsertStackMap names (index+1) varMap)
