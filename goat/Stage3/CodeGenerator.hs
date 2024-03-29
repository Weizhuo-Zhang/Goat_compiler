module CodeGenerator where

import           Analyzer
import           Control.Monad.State
import qualified Data.Map.Strict     as Map
import           GoatAST
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

-------------------------------------------------------------------------------
-- Alias for register.
-------------------------------------------------------------------------------
type Register = String

-------------------------------------------------------------------------------
-- Alias for slot number.
-------------------------------------------------------------------------------
type SlotNumber = Int

-------------------------------------------------------------------------------
-- Acceptable input value
-------------------------------------------------------------------------------
data InputValue = Int | Float | String


-------------------------------------------------------------------------------
-- The entry point for the code generation pipeline.
-------------------------------------------------------------------------------
codeGeneration :: ProgramMap -> IO ()
codeGeneration programMap = do
  printLine "call proc_main"
  printLine "halt"
  let procedures = Map.keys programMap
  generateProcedureList procedures programMap

-------------------------------------------------------------------------------
-- Generate a sequence of procedures from a list of procedure names.
-------------------------------------------------------------------------------
generateProcedureList :: [String] -> ProgramMap -> IO ()
generateProcedureList (procedure:[]) programMap =
  case Map.lookup procedure programMap of
    Just procedureTable -> do
      putStrLn $ "proc_" ++ procedure ++ ":"
      generateProcedure procedure procedureTable
      printLine "return"
generateProcedureList (procedure:procedures) programMap =
  case Map.lookup procedure programMap of
    Just procedureTable -> do
      putStrLn $ "proc_" ++ procedure ++ ":"
      generateProcedure procedure procedureTable
      printLine "return"
      generateProcedureList procedures programMap

-------------------------------------------------------------------------------
-- Generate an individual procedure for a given procedure name and the
-- corresponding PrecedureTable.
-------------------------------------------------------------------------------
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
      paramList = paramMapToList paramMap
  case parameterNumber of
    0         -> putStr ""
    otherwise -> do
      printComment "init parameters"
      initParameters paramList stackMap 0
  case variableNumber of
    0         -> putStr ""
    otherwise -> do
      printComment "init variables"
      initVariables varList varMap stackMap
  generateStatements procName [0] paramMap varMap statements stackMap
  case totalVarNumber of
    0         -> putStr ""
    otherwise -> printLine $ "pop_stack_frame " ++ (show totalVarNumber)

-------------------------------------------------------------------------------
-- Generate oz code for initializing a list of parameters.
-------------------------------------------------------------------------------
initParameters :: [Parameter] -> StackMap -> Int -> IO ()
initParameters [] _ _ = return ()
initParameters (param:[]) stackMap registerNum = do
  let paramSlotNum = stackMap Map.! paramName
      paramName = passingIdent param
  printComment $ "initialise parameters " ++ paramName
  printLine $ "store " ++ (show paramSlotNum) ++ ", r" ++ (show registerNum)

initParameters (param:params) stackMap registerNum = do
  let paramSlotNum = stackMap Map.! paramName
      paramName = passingIdent param
  printComment $ "initialise parameters " ++ paramName
  printLine $ "store " ++ (show paramSlotNum) ++ ", r" ++ (show registerNum)
  initParameters params stackMap (registerNum + 1)

-------------------------------------------------------------------------------
-- Generate oz code for initializing a list of variables.
-------------------------------------------------------------------------------
initVariables :: [Identifier] -> VariableMap -> StackMap -> IO ()
initVariables [] _ _ = return ()
initVariables (var:[]) varMap stackMap = do
  let varType    = declarationType $ varMap Map.! var
      varSlotNum   = stackMap Map.! var
      varIndicator = varShapeIndicator $ declarationVariable (varMap Map.! var)
  printComment $ "initialise variable " ++ var
  initVariableByBaseType varType
  initVariableWithIndicator varIndicator varSlotNum
initVariables (var:varList) varMap stackMap = do
  let varType    = declarationType $ varMap Map.! var
      varSlotNum = stackMap Map.! var
      varIndicator = varShapeIndicator $ declarationVariable (varMap Map.! var)
  printComment $ "init variable: " ++ var
  initVariableByBaseType varType
  initVariableWithIndicator varIndicator varSlotNum
  initVariables varList varMap stackMap

-------------------------------------------------------------------------------
-- Generate oz code for initialize a variable based on its base type, which
-- can be either float or int.
-------------------------------------------------------------------------------
initVariableByBaseType :: BaseType -> IO ()
initVariableByBaseType baseType =
  case baseType of
    FloatType -> printLine "real_const r0, 0.0"
    otherwise -> printLine "int_const r0, 0"

-------------------------------------------------------------------------------
-- Generate os code for initializing a variable that's either an array,
-- a matrix, or a value.
-------------------------------------------------------------------------------
initVariableWithIndicator :: ShapeIndicator -> Int -> IO ()
initVariableWithIndicator varIndicator varSlotNum = do
  case varIndicator of
    NoIndicator                      -> initSingleVar varSlotNum
    Array  (IntConst n)              -> initOffset    varSlotNum n
    Matrix (IntConst m) (IntConst n) -> initOffset    varSlotNum (m*n)

-------------------------------------------------------------------------------
-- Initialize a single value variable
-------------------------------------------------------------------------------
initSingleVar :: Int -> IO ()
initSingleVar varSlotNum = printLine $ "store " ++ (show varSlotNum) ++ ", r0"

-------------------------------------------------------------------------------
-- Helper function for initializing array or matrix.
-------------------------------------------------------------------------------
initOffset :: Int -> Int -> IO ()
initOffset varSlotNum offset = do
  if offset > 0 then do
    initSingleVar varSlotNum
    initOffset (varSlotNum+1) (offset-1)
  else putStr ""

-------------------------------------------------------------------------------
-- Generate oz code for a set of statements, given the list of StatementTables.
-------------------------------------------------------------------------------
generateStatements ::
  String -> [Int] -> ParameterMap -> VariableMap -> [StatementTable] ->
  StackMap -> IO ()
generateStatements _ _ _ _ [] _  = return ()
generateStatements procName label paramMap varMap (stat:[]) stackMap = do
  generateStatement procName (updateLabel label) paramMap varMap stat stackMap

generateStatements procName label paramMap varMap (stat:stats) stackMap = do
  generateStatement procName (updateLabel label) paramMap varMap stat stackMap
  generateStatements procName (updateLabel label) paramMap varMap stats
    stackMap


-------------------------------------------------------------------------------
-- Generate oz code for a single statement.
-------------------------------------------------------------------------------
generateStatement ::
  String -> [Int] -> ParameterMap -> VariableMap -> StatementTable ->
  StackMap -> IO ()
generateStatement procName label paramMap varMap statementTable stackMap = do
  case statementTable of
    AssignTable varTable  exprTable  -> generateAssignStatement
                                        procName
                                        paramMap
                                        varMap
                                        varTable
                                        exprTable
                                        stackMap
    WriteTable  exprTable            -> generateWriteStatement
                                        paramMap
                                        varMap
                                        exprTable
                                        stackMap
    IfTable     exprTable stmtTables -> generateIfStatement
                                        procName
                                        label
                                        exprTable
                                        paramMap
                                        varMap
                                        stmtTables
                                        stackMap
    IfElseTable exprTable stmtTables1 stmtTables2 -> generateIfElseStatement
                                                     procName
                                                     label
                                                     exprTable
                                                     paramMap
                                                     varMap
                                                     stmtTables1
                                                     stmtTables2
                                                     stackMap
    WhileTable  exprTable stmtTables -> generateWhileStatement
                                        procName
                                        label
                                        exprTable
                                        paramMap
                                        varMap
                                        stmtTables
                                        stackMap
    ReadTable exprTable -> generateReadStatement
                           exprTable
                           paramMap
                           varMap
                           stackMap
    CallTable procId expressionTables params -> generateCallStatement
                                                procId
                                                expressionTables
                                                params
                                                0
                                                paramMap
                                                varMap
                                                stackMap

-------------------------------------------------------------------------------
-- Genearate oz code for a procedure call statement.
-------------------------------------------------------------------------------
generateCallStatement ::
  Identifier -> [ExpressionTable] -> [Parameter] -> Int -> ParameterMap ->
  VariableMap -> StackMap -> IO ()
generateCallStatement procName [] [] _ _ _ _ = printLine
                                               ("call proc_" ++ procName)
generateCallStatement procName (exprTable:[]) (param:[]) registerNum paramMap
  varMap stackMap = do
    checkCallParameter param exprTable registerNum paramMap varMap stackMap
    -- print call statement after all parameters are loaded into registers.
    printLine $ "call proc_" ++ procName

generateCallStatement procName (exprTable:exprTables) (param:params)
  registerNum paramMap varMap stackMap = do
    checkCallParameter param exprTable registerNum paramMap varMap stackMap
    generateCallStatement procName exprTables params (registerNum+1) paramMap
      varMap stackMap

checkCallParameter ::
  Parameter -> ExpressionTable -> Int -> ParameterMap -> VariableMap ->
  StackMap -> IO ()
checkCallParameter param exprTable registerNum paramMap varMap stackMap = do
  let  varType   = getExpressionBaseType exprTable
       paramType = passingType param
       paramIndicator = passingIndicator param
  case paramIndicator of
    VarType -> do
      generateExpression paramMap varMap exprTable registerNum stackMap
      if (FloatType == paramType) && (IntType == varType)
        then printIntToRealInSameRegister 0
        else putStr ""
    RefType -> do
      let var = variable exprTable
          paramId = varName var
          slotNum = stackMap Map.! paramId
          slotNumStr = show slotNum
          regNumStr0 = "r" ++ (show registerNum)
      case (Map.member paramId paramMap) of
        False -> do
          let  varShape = varShapeIndicatorTable var
          case varShape of
            NoIndicatorTable ->  printLoadAddress regNumStr0 slotNumStr
            otherwise        -> do
              locateArrayMatrix paramMap varMap var slotNumStr registerNum
                stackMap
        True -> do
          let parameter = snd $ paramMap Map.! paramId
              passType  = passingIndicator parameter
          case passType of
            VarType -> printLoadAddress regNumStr0 slotNumStr
            RefType -> printLine $ "load " ++ regNumStr0 ++ ", " ++ slotNumStr

-------------------------------------------------------------------------------
-- Generate oz code for an assignment statement.
-------------------------------------------------------------------------------
generateAssignStatement ::
  String -> ParameterMap -> VariableMap -> ExpressionTable ->
  ExpressionTable -> StackMap -> IO ()
generateAssignStatement procName paramMap varMap varTable exprTable
  stackMap = do
    let var     = variable varTable
        varType = variableType varTable
        varId   = varName  var
        exprType = getExpressionBaseType exprTable
        varShape = varShapeIndicatorTable var
        varSlotNum = getVariableSlotNum var stackMap
        varSlotNumStr = show varSlotNum
    printComment $ "Assign statement for variable " ++ varId
    -- Right expression
    generateExpression paramMap varMap exprTable 0 stackMap
    if (FloatType == varType) && (IntType == exprType) then
      printIntToRealInSameRegister 0
    else putStr ""

    -- Left Variable
    case (Map.member varId paramMap) of
      False -> do
        case varShape of
          NoIndicatorTable -> printLine $ "store " ++ varSlotNumStr ++ ", r0"
          otherwise        -> do
            locateArrayMatrix paramMap varMap var varSlotNumStr 1 stackMap
            printLine $ "store_indirect r1, r0"

      True -> do
        let parameter = snd $ paramMap Map.! varId
            passType  = passingIndicator parameter
        case passType of
          VarType -> printLine $ "store " ++ varSlotNumStr ++ ", r0"
          RefType -> do
            printLine $ "load r1, " ++ varSlotNumStr
            printLine $ "store_indirect r1, r0"

-------------------------------------------------------------------------------
-- Generate oz code for a write statement.
-------------------------------------------------------------------------------
generateWriteStatement ::
  ParameterMap -> VariableMap -> ExpressionTable -> StackMap -> IO ()
generateWriteStatement paramMap varMap exprTable stackMap =
  case exprTable of
    VariableTable _ exprType -> generateWriteChooseType
                                exprType
                                paramMap
                                varMap
                                exprTable
                                stackMap
    BoolTable   _ -> generateWriteWithType
                     "bool"
                     paramMap
                     varMap
                     exprTable
                     stackMap
    IntTable    _ -> generateWriteWithType
                     "int"
                     paramMap
                     varMap
                     exprTable
                     stackMap
    FloatTable  _ -> generateWriteWithType
                     "real"
                     paramMap
                     varMap
                     exprTable
                     stackMap
    StringTable _ -> generateWriteWithType
                     "string"
                     paramMap
                     varMap
                     exprTable
                     stackMap
    AddTable    _ _ exprType -> generateWriteChooseType
                                exprType
                                paramMap
                                varMap
                                exprTable
                                stackMap
    SubTable    _ _ exprType -> generateWriteChooseType
                                exprType
                                paramMap
                                varMap
                                exprTable
                                stackMap
    MulTable    _ _ exprType -> generateWriteChooseType
                                exprType
                                paramMap
                                varMap
                                exprTable
                                stackMap
    DivTable    _ _ exprType -> generateWriteChooseType
                                exprType
                                paramMap
                                varMap
                                exprTable
                                stackMap
    NegativeTable _ exprType -> generateWriteChooseType
                                exprType
                                paramMap
                                varMap
                                exprTable
                                stackMap
    otherwise -> generateWriteWithType
                 "bool"
                 paramMap
                 varMap
                 exprTable
                 stackMap

-------------------------------------------------------------------------------
-- Generate oz code for a write statement of a given type as a string.
-------------------------------------------------------------------------------
generateWriteWithType ::
  String -> ParameterMap -> VariableMap -> ExpressionTable -> StackMap -> IO ()
generateWriteWithType writeType paramMap varMap exprTable stackMap = do
  generateExpression paramMap varMap exprTable 0 stackMap
  printLine $ "call_builtin print_" ++ writeType

-------------------------------------------------------------------------------
-- Generate oz code for a write statement of a given base type.
-------------------------------------------------------------------------------
generateWriteChooseType ::
  BaseType -> ParameterMap -> VariableMap -> ExpressionTable -> StackMap ->
  IO ()
generateWriteChooseType exprType paramMap varMap exprTable stackMap =
  case exprType of
    IntType   -> generateWriteWithType
                 "int"
                 paramMap
                 varMap
                 exprTable
                 stackMap
    FloatType -> generateWriteWithType
                 "real"
                 paramMap
                 varMap
                 exprTable
                 stackMap
    BoolType  -> generateWriteWithType
                 "bool"
                 paramMap
                 varMap
                 exprTable
                 stackMap

-------------------------------------------------------------------------------
-- Generate oz code for a read statement.
-------------------------------------------------------------------------------
generateReadStatement ::
  ExpressionTable -> ParameterMap -> VariableMap -> StackMap -> IO ()
generateReadStatement exprTable paramMap varMap stackMap = do
  let exprType = getExpressionBaseType exprTable
      slotNum = stackMap Map.! (varName $ variable exprTable)
  case exprType of
    BoolType  -> generateReadStatementByType
                 "bool"
                 slotNum
                 exprTable
                 paramMap
                 varMap
                 stackMap
    IntType   -> generateReadStatementByType
                 "int"
                 slotNum
                 exprTable
                 paramMap
                 varMap
                 stackMap
    FloatType -> generateReadStatementByType
                 "real"
                 slotNum
                 exprTable
                 paramMap
                 varMap
                 stackMap

-------------------------------------------------------------------------------
-- Generate oz code for a read statement of a given base type.
-------------------------------------------------------------------------------
generateReadStatementByType ::
  String -> Int -> ExpressionTable -> ParameterMap -> VariableMap ->
  StackMap -> IO ()
generateReadStatementByType baseType slotNum exprTable paramMap varMap
  stackMap = do
    case exprTable of
      VariableTable var varType -> do
        let varId   = varName  var
            varShape = varShapeIndicatorTable var
            varSlotNum = getVariableSlotNum var stackMap
            varSlotNumStr = show varSlotNum
        printLine ("call_builtin read_" ++ baseType)
        case (Map.member varId paramMap) of
          False -> do
            case varShape of
              NoIndicatorTable ->
                printLine $ "store " ++ varSlotNumStr ++ ", r0"
              otherwise        -> do
                locateArrayMatrix paramMap varMap var varSlotNumStr 1 stackMap
                printLine $ "store_indirect r1, r0"
          True -> do
            let parameter = snd $ paramMap Map.! varId
                passType  = passingIndicator parameter
            case passType of
              VarType -> printLine $ "store " ++ varSlotNumStr ++ ", r0"
              RefType -> do
                printLine $ "load r1, " ++ varSlotNumStr
                printLine $ "store_indirect r1, r0"

-------------------------------------------------------------------------------
-- Generate oz code for an expression, given its ExpressionTable.
-------------------------------------------------------------------------------
generateExpression ::
  ParameterMap -> VariableMap -> ExpressionTable -> Int -> StackMap -> IO ()
generateExpression paramMap varMap exprTable registerNum stackMap =
  case exprTable of
    VariableTable var varType ->
      generateVariableExpr paramMap varMap var varType registerNum stackMap
    BoolTable val -> printLine ("int_const r" ++ (show registerNum) ++
                                ", " ++ (convertBoolToInt val))
    IntTable val -> do
      printNewLineIndentation
      putStrLn $ "int_const r" ++ (show registerNum) ++ ", " ++ (show val)
    FloatTable val -> do
      printNewLineIndentation
      putStrLn $ "real_const r" ++ (show registerNum) ++ ", " ++ (show val)
    StringTable val -> printLine ("string_const r" ++ (show registerNum) ++
                                  ", " ++ (wrapWithDoubleQuotations val))
    AddTable lExpr rExpr baseType -> do
      generateExpression paramMap varMap lExpr registerNum stackMap
      generateExpression paramMap varMap rExpr (registerNum+1) stackMap
      case baseType of
        IntType -> generateOperationString "add" "int" registerNum
        FloatType -> do
          generateIntToFloat lExpr rExpr registerNum
          generateOperationString "add" "real" registerNum
    SubTable lExpr rExpr baseType -> do
      generateExpression paramMap varMap lExpr registerNum stackMap
      generateExpression paramMap varMap rExpr (registerNum+1) stackMap
      case baseType of
        IntType -> generateOperationString "sub" "int" registerNum
        FloatType -> do
          generateIntToFloat lExpr rExpr registerNum
          generateOperationString "sub" "real" registerNum
    MulTable lExpr rExpr baseType -> do
      generateExpression paramMap varMap lExpr registerNum stackMap
      generateExpression paramMap varMap rExpr (registerNum+1) stackMap
      case baseType of
        IntType -> generateOperationString "mul" "int" registerNum
        FloatType -> do
          generateIntToFloat lExpr rExpr registerNum
          generateOperationString "mul" "real" registerNum
    DivTable lExpr rExpr baseType -> do
      generateExpression paramMap varMap lExpr registerNum stackMap
      generateExpression paramMap varMap rExpr (registerNum+1) stackMap
      case baseType of
        IntType -> generateOperationString "div" "int" registerNum
        FloatType -> do
          generateIntToFloat lExpr rExpr registerNum
          generateOperationString "div" "real" registerNum
    OrTable    lExpr rExpr _        -> generateOrExpression
                                       paramMap
                                       varMap
                                       lExpr
                                       rExpr
                                       registerNum
                                       stackMap
    AndTable   lExpr rExpr _        -> generateAndExpression
                                       paramMap
                                       varMap
                                       lExpr
                                       rExpr
                                       registerNum
                                       stackMap
    EqTable    lExpr rExpr exprType -> generateEqExpression
                                       paramMap
                                       varMap
                                       lExpr
                                       rExpr
                                       registerNum
                                       exprType
                                       stackMap
    NotEqTable lExpr rExpr exprType -> generateNotEqExpression
                                       paramMap
                                       varMap
                                       lExpr
                                       rExpr
                                       registerNum
                                       exprType
                                       stackMap
    LesTable   lExpr rExpr exprType -> generateLesExpression
                                       paramMap
                                       varMap
                                       lExpr
                                       rExpr
                                       registerNum
                                       exprType
                                       stackMap
    LesEqTable lExpr rExpr exprType -> generateLesEqExpression
                                       paramMap
                                       varMap
                                       lExpr
                                       rExpr
                                       registerNum
                                       exprType
                                       stackMap
    GrtTable   lExpr rExpr exprType -> generateGrtExpression
                                       paramMap
                                       varMap
                                       lExpr
                                       rExpr
                                       registerNum
                                       exprType
                                       stackMap
    GrtEqTable lExpr rExpr exprType -> generateGrtEqExpression
                                       paramMap
                                       varMap
                                       lExpr
                                       rExpr
                                       registerNum
                                       exprType
                                       stackMap
    NegativeTable     expr exprType -> generateNegativeExpression
                                       paramMap
                                       varMap
                                       expr
                                       registerNum
                                       exprType
                                       stackMap
    NotTable   expr  _ -> generateNotExpression
                          paramMap
                          varMap
                          expr
                          registerNum
                          stackMap

-------------------------------------------------------------------------------
-- Update labels for statement generation.
-------------------------------------------------------------------------------
updateLabel :: [Int] -> [Int]
updateLabel (x:[]) = (x+1):[]
updateLabel (x:xs) = x:(updateLabel xs)

-------------------------------------------------------------------------------
-- Helper function to print labels for conditional statements.
-------------------------------------------------------------------------------
showLabel :: [Int] -> String
showLabel (x:[]) = show(x)
showLabel (x:xs) = show(x) ++ "_" ++ showLabel(xs)

-------------------------------------------------------------------------------
-- Generate oz code for an if statement.
-------------------------------------------------------------------------------
generateIfStatement ::
  String -> [Int] -> ExpressionTable -> ParameterMap -> VariableMap ->
  [StatementTable] -> StackMap -> IO ()
generateIfStatement procName label exprTable paramMap varMap stmts
  stackMap = do
    let label_a = procName ++ "_" ++ (showLabel label) ++ "_a"
    let label_b = procName ++ "_" ++ (showLabel label) ++ "_b"
    -- check condition
    generateExpression paramMap varMap exprTable 0 stackMap
    printLine ("branch_on_true r0, " ++ label_a)
    printLine ("branch_uncond " ++ label_b)
    -- If statements
    putStrLn (label_a ++ ":")
    generateStatements procName (label ++ [0] ++ [0]) paramMap varMap stmts
      stackMap
    -- end of this statements
    putStrLn (label_b ++ ":")

-------------------------------------------------------------------------------
-- Generate oz code for an if-else statement
-------------------------------------------------------------------------------
generateIfElseStatement ::
  String -> [Int] -> ExpressionTable -> ParameterMap -> VariableMap ->
  [StatementTable] -> [StatementTable] -> StackMap -> IO ()
generateIfElseStatement procName label exprTable paramMap varMap stmts1 stmts2
  stackMap = do
    let label_a = procName ++ "_" ++ (showLabel label) ++ "_a"
    let label_b = procName ++ "_" ++ (showLabel label) ++ "_b"
    generateExpression paramMap varMap exprTable 0 stackMap
    -- Else statements
    printLine ("branch_on_false r0, " ++ label_a)
    -- If statements
    generateStatements procName (label ++ [1] ++ [0]) paramMap varMap stmts1
      stackMap
    printLine ("branch_uncond " ++ label_b)
    -- Else statements
    putStrLn (label_a ++ ":")
    generateStatements procName (label ++ [2] ++ [0]) paramMap varMap stmts2
      stackMap
    -- fi The end of If-Else
    putStrLn (label_b ++ ":")

-------------------------------------------------------------------------------
-- Generate oz code for a while statement.
-------------------------------------------------------------------------------
generateWhileStatement ::
  String -> [Int] -> ExpressionTable -> ParameterMap -> VariableMap ->
  [StatementTable] -> StackMap -> IO ()
generateWhileStatement procName label exprTable paramMap varMap stmts
  stackMap = do
    let label_a = procName ++ "_" ++ (showLabel label) ++ "_a"
    let label_b = procName ++ "_" ++ (showLabel label) ++ "_b"
    let label_c = procName ++ "_" ++ (showLabel label) ++ "_c"
    -- check condition
    putStrLn (label_a ++ ":")
    generateExpression paramMap varMap exprTable 0 stackMap
    printLine ("branch_on_true r0, " ++ label_b)
    printLine ("branch_uncond " ++ label_c)
    -- while statements
    putStrLn (label_b ++ ":")
    generateStatements procName (label ++ [3] ++ [0]) paramMap varMap stmts
      stackMap
    -- check condition again
    printLine ("branch_uncond " ++ label_a)
    -- end of this while loop
    putStrLn (label_c ++ ":")

-------------------------------------------------------------------------------
-- Generate oz code for variable whose value is an expression.
-------------------------------------------------------------------------------
generateVariableExpr ::
  ParameterMap -> VariableMap -> VariableSubTable -> BaseType -> Int ->
  StackMap -> IO ()
generateVariableExpr paramMap varMap var varType regNum stackMap = do
  let varShape = varShapeIndicatorTable var
      varSlotNum = getVariableSlotNum var stackMap
      variableName = varName var
      varSlotNumStr = show varSlotNum
      regNumStr0 = "r" ++ (show regNum)
  case (Map.member variableName paramMap) of
    False -> do
      case varShape of
        NoIndicatorTable -> printLoadValue regNumStr0 varSlotNumStr
        otherwise        -> do
          locateArrayMatrix paramMap varMap var varSlotNumStr regNum stackMap
          printLine $ "load_indirect " ++ regNumStr0 ++ ", " ++ regNumStr0
    True -> do
      let parameter = snd $ paramMap Map.! variableName
          passType  = passingIndicator parameter
      case passType of
        VarType -> printLoadValue regNumStr0 varSlotNumStr
        RefType -> do
          printLoadValue regNumStr0 varSlotNumStr
          printLine $ "load_indirect " ++ regNumStr0 ++ ", " ++ regNumStr0

-------------------------------------------------------------------------------
-- Locate register of the value of a cell of an array/matrix for passing by ref.
-------------------------------------------------------------------------------
locateArrayMatrix ::
  ParameterMap -> VariableMap -> VariableSubTable -> String -> Int ->
  StackMap -> IO ()
locateArrayMatrix paramMap varMap var varSlotNumStr regNum stackMap = do
  let varShape = varShapeIndicatorTable var
      regNumStr0 = "r" ++ (show regNum)
      regNumStr1 = "r" ++ (show $ regNum + 1)
  case varShape of
    NoIndicatorTable -> printLoadAddress regNumStr0 varSlotNumStr
    ArrayTable  expr -> do
      printComment $ "Generate Array " ++ (varName var)
      generateExpression paramMap varMap expr regNum stackMap
      printLoadAddress regNumStr1 varSlotNumStr
      printLine $ "sub_offset "   ++ regNumStr0 ++ ", " ++
                  regNumStr1 ++ ", " ++ regNumStr0
    MatrixTable exprM exprN -> do
      let varId    = varName var
          varDeclShape = varShapeIndicator
                           (declarationVariable (varMap Map.! varId))
          m = show $ getMatrixM varDeclShape
      printComment $ "Generate Matrix " ++ varId
      generateExpression paramMap varMap exprM regNum stackMap
      printLine $ "int_const " ++ regNumStr1 ++ ", " ++ m
      printLine $ "mul_int " ++ regNumStr0 ++ ", " ++ regNumStr0 ++
                  ", " ++ regNumStr1
      generateExpression paramMap varMap exprN (regNum+1) stackMap
      printLine $ "add_int "  ++ regNumStr0 ++ ", " ++ regNumStr1 ++
                  ", " ++ regNumStr0
      printLoadAddress regNumStr1 varSlotNumStr
      printLine $ "sub_offset "  ++ regNumStr0 ++ ", " ++ regNumStr1 ++
                  ", " ++ regNumStr0


-------------------------------------------------------------------------------
-- Get the row number of a matrix.
-------------------------------------------------------------------------------
getMatrixM :: ShapeIndicator -> Int
getMatrixM (Matrix (IntConst n) _ ) = n
  -- case m of
  --   IntConst n -> n

-------------------------------------------------------------------------------
-- Generate oz code for || expression.
-------------------------------------------------------------------------------
generateOrExpression ::
  ParameterMap -> VariableMap -> ExpressionTable -> ExpressionTable ->
  Int -> StackMap -> IO ()
generateOrExpression paramMap varMap lExpr rExpr regNum stackMap = do
  generateAndOrExpr "or" paramMap varMap lExpr rExpr regNum stackMap

-------------------------------------------------------------------------------
-- Generate oz code for && expression.
-------------------------------------------------------------------------------
generateAndExpression ::
  ParameterMap -> VariableMap -> ExpressionTable -> ExpressionTable ->
  Int -> StackMap -> IO ()
generateAndExpression paramMap varMap lExpr rExpr regNum stackMap = do
  generateAndOrExpr "and" paramMap varMap lExpr rExpr regNum stackMap

-------------------------------------------------------------------------------
-- Generate oz code for AND-OR expression.
-------------------------------------------------------------------------------
generateAndOrExpr ::
  String -> ParameterMap -> VariableMap -> ExpressionTable ->
  ExpressionTable -> Int -> StackMap -> IO ()
generateAndOrExpr operator paramMap varMap lExpr rExpr regNum stackMap = do
  { generateExpression paramMap varMap lExpr regNum stackMap
  ; generateExpression paramMap varMap rExpr (regNum+1) stackMap
  ; printLine $ operator ++ " r" ++ (show regNum) ++ ", r" ++ (show regNum) ++
                ", r" ++ (show (regNum+1))
  }

-------------------------------------------------------------------------------
-- Generate oz code for ! expression.
-------------------------------------------------------------------------------
generateNotExpression ::
  ParameterMap -> VariableMap -> ExpressionTable -> Int -> StackMap -> IO ()
generateNotExpression paramMap varMap expr regNum stackMap = do
  generateExpression paramMap varMap expr regNum stackMap
  printLine $ "not r" ++ (show regNum) ++ ", r" ++ (show regNum)

-------------------------------------------------------------------------------
-- Generate oz code for = expression.
-------------------------------------------------------------------------------
generateEqExpression ::
  ParameterMap -> VariableMap -> ExpressionTable -> ExpressionTable -> Int ->
  BaseType -> StackMap -> IO ()
generateEqExpression paramMap varMap lExpr rExpr regNum exprType stackMap = do
  generateCompareExpr "cmp_eq" paramMap varMap lExpr rExpr regNum exprType stackMap

-------------------------------------------------------------------------------
-- Generate oz code for != expression.
-------------------------------------------------------------------------------
generateNotEqExpression ::
  ParameterMap -> VariableMap -> ExpressionTable -> ExpressionTable ->
  Int -> BaseType -> StackMap -> IO ()
generateNotEqExpression paramMap varMap lExpr rExpr regNum exprType
  stackMap = do
    generateCompareExpr "cmp_ne" paramMap varMap lExpr rExpr regNum exprType
      stackMap

-------------------------------------------------------------------------------
-- Generate oz code for < expression.
-------------------------------------------------------------------------------
generateLesExpression ::
  ParameterMap -> VariableMap -> ExpressionTable -> ExpressionTable ->
  Int -> BaseType -> StackMap -> IO ()
generateLesExpression paramMap varMap lExpr rExpr regNum exprType
  stackMap = do
    generateCompareExpr "cmp_lt" paramMap varMap lExpr rExpr regNum exprType
      stackMap

-------------------------------------------------------------------------------
-- Generate oz code for <= expression.
-------------------------------------------------------------------------------
generateLesEqExpression ::
  ParameterMap -> VariableMap -> ExpressionTable -> ExpressionTable ->
  Int -> BaseType -> StackMap -> IO ()
generateLesEqExpression paramMap varMap lExpr rExpr regNum exprType
  stackMap = do
    generateCompareExpr "cmp_le" paramMap varMap lExpr rExpr regNum exprType
      stackMap

-------------------------------------------------------------------------------
-- Generate oz code for > expression.
-------------------------------------------------------------------------------
generateGrtExpression ::
  ParameterMap -> VariableMap -> ExpressionTable -> ExpressionTable -> Int ->
  BaseType -> StackMap -> IO ()
generateGrtExpression paramMap varMap lExpr rExpr regNum exprType
  stackMap = do
    generateCompareExpr "cmp_gt" paramMap varMap lExpr rExpr regNum exprType
      stackMap

-------------------------------------------------------------------------------
-- Generate oz code for >= expression.
-------------------------------------------------------------------------------
generateGrtEqExpression ::
  ParameterMap -> VariableMap -> ExpressionTable -> ExpressionTable -> Int ->
  BaseType -> StackMap -> IO ()
generateGrtEqExpression paramMap varMap lExpr rExpr regNum exprType
  stackMap = do
    generateCompareExpr "cmp_ge" paramMap varMap lExpr rExpr regNum exprType
      stackMap

-------------------------------------------------------------------------------
-- Generate comparison expression of a given type, which can be:
-- =, >, <, >=, <=
-------------------------------------------------------------------------------
generateCompareExpr ::
  String -> ParameterMap -> VariableMap -> ExpressionTable ->
  ExpressionTable -> Int -> BaseType -> StackMap -> IO ()
generateCompareExpr operator paramMap varMap lExpr rExpr regNum exprType
  stackMap = do
    generateExpression paramMap varMap lExpr regNum stackMap
    generateExpression paramMap varMap rExpr (regNum+1) stackMap
    case exprType of
      FloatType -> do
        generateIntToFloat lExpr rExpr regNum
        printLine $ operator ++ "_real r" ++ (show regNum) ++ ", r" ++
                    (show regNum) ++ ", r" ++ (show (regNum+1))
      otherwise ->
        printLine $ operator ++ "_int r" ++ (show regNum) ++ ", r" ++
                    (show regNum) ++ ", r" ++ (show (regNum+1))

-------------------------------------------------------------------------------
-- Generate oz code for expressions with a single unary operator -.
-------------------------------------------------------------------------------
generateNegativeExpression ::
  ParameterMap -> VariableMap -> ExpressionTable -> Int -> BaseType ->
  StackMap -> IO ()
generateNegativeExpression paramMap varMap expr registerNum exprType
  stackMap = do
    let regNum = show registerNum
    generateExpression paramMap varMap expr registerNum stackMap
    case exprType of
      IntType   -> printLine $ "neg_int r"  ++ regNum ++ ", r" ++ regNum
      FloatType -> printLine $ "neg_real r" ++ regNum ++ ", r" ++ regNum


-------------------------------------------------------------------------------
-- Convert boolean to int.
-------------------------------------------------------------------------------
convertBoolToInt :: Bool -> String
convertBoolToInt boolVal =
  case boolVal of
    True  -> show 1
    False -> show 0

-------------------------------------------------------------------------------
-- Print a string with indentation.
-------------------------------------------------------------------------------
printLine :: String -> IO ()
printLine string = do
  printNewLineIndentation
  putStrLn string

-------------------------------------------------------------------------------
-- Generate oz code for comments.
-------------------------------------------------------------------------------
printComment :: String -> IO ()
printComment string = do
  putStr "  # "
  putStrLn string

-------------------------------------------------------------------------------
-- Register
-------------------------------------------------------------------------------
registers :: Map.Map register inputValue
registers = Map.empty

------------------------------- Helper functions ------------------------------
printLoadAddress :: String -> String -> IO ()
printLoadAddress regNumStr0 slotNumStr =
  printLine $ "load_address " ++ regNumStr0 ++ ", " ++ slotNumStr

printLoadValue :: String -> String -> IO ()
printLoadValue regNumStr0 varSlotNumStr =
  printLine $ "load " ++ regNumStr0 ++ ", " ++ varSlotNumStr

-------------------------------------------------------------------------------
-- Start a new line with proper indentation.
-------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------
-- Generate string for binary operators.
-------------------------------------------------------------------------------
generateOperationString :: String -> String -> Int -> IO ()
generateOperationString operator opType registerNum = do
  printNewLineIndentation
  putStrLn $ operator ++ "_" ++ opType ++ " r" ++ (show registerNum)
              ++ ", r" ++ (show registerNum) ++ ", r" ++ (show $ registerNum+1)

-------------------------------------------------------------------------------
-- Convert int type to float type.
-------------------------------------------------------------------------------
generateIntToFloat :: ExpressionTable -> ExpressionTable -> Int -> IO ()
generateIntToFloat lExpr rExpr registerNum = do
  let lType = getExpressionBaseType lExpr
      rType = getExpressionBaseType rExpr
  case (lType,rType) of
    (FloatType,FloatType) -> return ()
    (IntType,FloatType)   -> printIntToRealInSameRegister registerNum
    (FloatType,IntType)   -> printIntToRealInSameRegister (registerNum + 1)

-------------------------------------------------------------------------------
-- Get a non-conflicting slot number for a new variable.
-------------------------------------------------------------------------------
getVariableSlotNum :: VariableSubTable -> StackMap -> Int
getVariableSlotNum variable stackMap = stackMap Map.! varId
  where varId = varName variable

-------------------------------------------------------------------------------
-- Get the size of VariableMap.
-------------------------------------------------------------------------------
getVariableMapSize :: [Identifier] -> VariableMap -> Int
getVariableMapSize [] _ = 0
getVariableMapSize (var:[]) varMap = getVariableSize varIndicator
  where varIndicator = varShapeIndicator
                       (declarationVariable (varMap Map.! var))
getVariableMapSize (var:vars) varMap =
  (getVariableSize varIndicator) + (getVariableMapSize vars varMap)
  where varIndicator = varShapeIndicator
                       (declarationVariable (varMap Map.! var))

-------------------------------------------------------------------------------
-- Get the size of a variable.
-- Array/matrix: the size of object
-- Neither: 1
-------------------------------------------------------------------------------
getVariableSize :: ShapeIndicator -> Int
getVariableSize varIndicator =
  case varIndicator of
    NoIndicator                      -> 1
    Array  (IntConst n)              -> n
    Matrix (IntConst m) (IntConst n) -> m*n

-------------------------------------------------------------------------------
-- Generate a StackMap, given the parameters and variables of a procedure.
-------------------------------------------------------------------------------
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
        where varIndicator = varShapeIndicator
                             (declarationVariable (varMap Map.! name))
              varSize = getVariableSize varIndicator
    False -> Map.insert name index Map.empty
subinsertStackMap (name:names) index varMap =
  case (Map.member name varMap) of
    True  -> Map.insert
             name
             index
             (subinsertStackMap names (index+varSize) varMap)
        where varIndicator = varShapeIndicator
                             (declarationVariable (varMap Map.! name))
              varSize = getVariableSize varIndicator
    False -> Map.insert name index (subinsertStackMap names (index+1) varMap)
