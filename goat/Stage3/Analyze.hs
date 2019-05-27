module Analyze where

import qualified Data.Map.Strict as M
import           Data.Maybe
import           GoatAST
import           GoatConstant
import           GoatExit
import           SymbolTable
import           Util

-------------------------------- Documentation --------------------------------

-- Authors:
--   Shizhe Cai (shizhec) - 798125
--   Weizhuo Zhang (weizhuoz) - 1018329
--   Mingyang Zhang (mingyangz) - 650242
--   An Luo (aluo1) - 657605

-- This file contains the codes that analyze AST's semantics.

-- The aim of the project is to implement a compiler for a procedural (C-like)
-- language called Goat.

-------------------------------- Documentation --------------------------------

-------------------------------- Utility Code ---------------------------------
-------------------------------------------------------------------------------
-- lookup parameter Map, It must have a base type
-------------------------------------------------------------------------------
lookupBaseTypeParamMap :: Identifier -> M.Map Identifier Parameter -> BaseType
lookupBaseTypeParamMap varName paramMap =
    case M.lookup varName paramMap of
      Just parameter -> passingType parameter

-------------------------------------------------------------------------------
-- lookup variable Map, It must have a base type
-------------------------------------------------------------------------------
lookupBaseTypeVarMap :: Identifier -> M.Map Identifier VariableDeclaration -> BaseType
lookupBaseTypeVarMap varName varMap =
      case M.lookup varName varMap of
        Just variable -> declarationType variable

-------------------------------------------------------------------------------
-- Get procedure identifier from procedure.
-------------------------------------------------------------------------------
getProcedureIdentifier :: Procedure -> Identifier
getProcedureIdentifier = headerIdent . header

-------------------------------------------------------------------------------
-- Get procedure identifier from procedure.
-------------------------------------------------------------------------------
getProcedureParameters :: Procedure -> [Parameter]
getProcedureParameters = parameters . header

getMultipleVarDeclarationErrorMessage :: Identifier -> Identifier -> String
getMultipleVarDeclarationErrorMessage varName procName =
  "There are multiple variable declaration named " ++
  "\"" ++ varName ++ "\"" ++ " in procedure " ++
  "\"" ++ procName ++ "\""

exitWithMultipleVarDeclaration :: Identifier -> Identifier -> IO Task
exitWithMultipleVarDeclaration varName procName =
  exitWithError
  (getMultipleVarDeclarationErrorMessage varName procName)
  MultipleVar

getConditionTypeErrorMessage :: Identifier -> String
getConditionTypeErrorMessage procName =
  "If condition type error! The type must be bool. In procedure " ++
  "\"" ++ procName ++ "\""

exitWithConditionTypeError :: Identifier -> IO Task
exitWithConditionTypeError procName =
  exitWithError
  (getConditionTypeErrorMessage procName)
  IfCondError

getLogicExprTypeErrorMessage :: Identifier -> String -> String
getLogicExprTypeErrorMessage procName operator =
  "\"" ++ operator ++ "\" type error! The argument of \"" ++ operator ++
  "\" must be bool. In procedure " ++
  "\"" ++ procName ++ "\""

exitWithLogicExprTypeError :: Identifier -> String -> IO Task
exitWithLogicExprTypeError procName operator =
  exitWithError
  (getLogicExprTypeErrorMessage procName operator)
  LogicOpTypeError

getUndefinedVariableErrorMessage :: Identifier -> String
getUndefinedVariableErrorMessage varName =
  "There is a undefined variable named " ++
  "\"" ++ varName ++ "\"" ++
  " in the statement"

exitWithUndefinedVariable :: Identifier -> IO Task
exitWithUndefinedVariable varName =
  exitWithError
  (getUndefinedVariableErrorMessage varName)
  UndefinedVar

exitWithReadIncorrect :: IO Task
exitWithReadIncorrect =
  exitWithError "Cannot read into non-variable" ReadIncorrect

exitWithTypeError :: Identifier -> IO Task
exitWithTypeError procName =
  exitWithError ("There is a Type Error in the Statment in proc: " ++
                "\"" ++ procName ++ "\"") UnmatchedType

getComparisonExprTypeErrorMessage :: Identifier -> String -> String
getComparisonExprTypeErrorMessage procName operator =
  "\"" ++ operator ++ "\" type error! The argument of \"" ++ operator ++
  "\" must be base type. In procedure " ++
  "\"" ++ procName ++ "\""

exitWithComparisonTypeError :: Identifier -> String -> IO Task
exitWithComparisonTypeError procName operator =
  exitWithError
  (getComparisonExprTypeErrorMessage procName operator)
  ComparisonError

getNotSameTypeErrorMessage :: Identifier -> String -> String
getNotSameTypeErrorMessage procName operator =
  "\"" ++ operator ++ "\" type error! The argument of \"" ++ operator ++
  "\" must be same base type. In procedure " ++
  "\"" ++ procName ++ "\""

exitWithNotSameTypeError :: Identifier -> String -> IO Task
exitWithNotSameTypeError procName operator =
  exitWithError
  (getNotSameTypeErrorMessage procName operator)
  NotSameTypeError

getUnaryMinusTypeErrorMessage :: Identifier -> String
getUnaryMinusTypeErrorMessage procName =
  "\"-\" (Unary Minus) type error! The argument of \"-\" (Unary Minus)" ++
  " must be int or float. In procedure " ++
  "\"" ++ procName ++ "\""

exitWithUnaryMinusError :: Identifier -> IO Task
exitWithUnaryMinusError procName =
  exitWithError
  (getUnaryMinusTypeErrorMessage procName)
  UnaryMinusError

getAssignTypeErrorMessage :: Identifier -> String -> String
getAssignTypeErrorMessage procName varName =
  "Assign Type Error! The type of " ++ "\"" ++ varName ++ "\"" ++
  " in procedure " ++ "\"" ++ procName ++ "\"" ++ " is not match."

exitWithAssignTypeError :: Identifier -> String -> IO Task
exitWithAssignTypeError procName varName =
  exitWithError
  (getAssignTypeErrorMessage procName varName)
  AssignTypeError

-------------------------------- Analyzer Code --------------------------------

-------------------------------------------------------------------------------
-- analyze Procedure
-------------------------------------------------------------------------------
analyze :: ProgramMap -> IO Task
analyze _           = return Unit
-- analyzeProc (proc:[])    = do { printHeader $ header proc
--                               ; printBody $ body proc
--                               }
-- analyzeProc (proc:procs) = do { printHeader $ header proc
--                               ; printBody $ body proc
--                               ; putStrLn "" -- print new line character
--                               ; printProc (procs)
--                               }

insertProcList :: [Procedure] -> ProgramMap -> Either (IO Task) ProgramMap
insertProcList (proc:[]) procMap = do
    let procTable = insertProcedureTable proc
    case procTable of
        Left err           -> Left err
        Right subProcTable -> Right $ M.insert procName subProcTable procMap
    where procName = getProcedureIdentifier proc
insertProcList (proc:procs) procMap = do
  let newProcMap = insertProcList procs procMap
  case newProcMap of
    Left err -> Left err
    Right subProcMap -> do
      let procName = getProcedureIdentifier proc
      case (M.member procName subProcMap) of
        True  ->
          Left $ exitWithError
                 ( "There are multiple procedures named " ++
                   "\"" ++ procName ++ "\""
                 )
                 MultipleProc
        False -> do
          let procTable = insertProcedureTable proc
          case procTable of
            Left err -> Left err
            Right subProcTable ->
              Right $ M.insert procName subProcTable subProcMap

insertProcedureTable :: Procedure -> Either (IO Task) ProcedureTable
insertProcedureTable procedure =
  case paramMap of
          Left err -> Left err
          Right subParamMap -> do
              let varMap =
                      insertVariableMap
                          procedureName (bodyVarDeclarations procedureBody) subParamMap M.empty
              case varMap of
                  Left err -> Left err
                  Right subVarMap -> do
                      let newStatements = insertStatementList procedureName (bodyStatements procedureBody) subParamMap subVarMap
                      case newStatements of
                          Left err -> Left err
                          Right subStatements ->
                              Right $ ProcedureTable subParamMap subVarMap subStatements

 where paramMap = insertParameterMap procedureName procedureParameters M.empty
       procedureBody = (body procedure)
       procedureName = getProcedureIdentifier procedure
       procedureParameters = getProcedureParameters procedure

insertParameterMap ::
  Identifier -> [Parameter] -> ParameterMap -> Either (IO Task) ParameterMap
insertParameterMap procName [] paramMap = Right paramMap
insertParameterMap procName (param:[]) paramMap =
  Right $ M.insert (passingIdent param) param paramMap
insertParameterMap procName (param:params) paramMap = do
  let newParamMap = insertParameterMap procName params paramMap
  case newParamMap of
    Left err -> Left err
    Right subParamMap -> do
      let paramName = (passingIdent param)
      case (M.member paramName subParamMap) of
        True  -> Left $ exitWithMultipleVarDeclaration paramName procName
        False -> Right $ M.insert paramName param subParamMap


insertVariableMap ::
  Identifier -> [VariableDeclaration] -> ParameterMap -> VariableMap -> Either (IO Task) VariableMap
insertVariableMap procName [] paramMap varMap = Right varMap
insertVariableMap procName (bodyVarDecl:[]) paramMap varMap = do
  let varName = varId $ declarationVariable bodyVarDecl
  case (M.member varName paramMap) of
      True  -> Left $ exitWithMultipleVarDeclaration varName procName
      False -> Right $ M.insert varName bodyVarDecl varMap
insertVariableMap procName (bodyVarDecl:bodyVarDecls) paramMap varMap = do
    let varName = varId $ declarationVariable bodyVarDecl
    case (M.member varName paramMap) of
        True  -> Left $ exitWithMultipleVarDeclaration varName procName
        False -> do
            let newVarMap = insertVariableMap procName bodyVarDecls paramMap varMap
            case newVarMap of
                Left err -> Left err
                Right subVarMap -> do
                    case (M.member varName subVarMap) of
                        True  -> Left $ exitWithMultipleVarDeclaration varName procName
                        False -> Right $ M.insert varName bodyVarDecl subVarMap

insertStatementList :: Identifier -> [Statement] -> ParameterMap -> VariableMap -> Either (IO Task) [StatementTable]
insertStatementList procName (stmt:[]) paramMap varMap = do
    let newStmtTable = checkStatement procName stmt paramMap varMap
    case newStmtTable of
        Left err        -> Left err
        Right stmtTable -> Right $ (stmtTable):[]
insertStatementList procName (stmt:stmts) paramMap varMap = do
    let newStatements = insertStatementList procName stmts paramMap varMap
    case newStatements of
        Left err            -> Left err
        Right subStatements -> do
            let newStmtTable = checkStatement procName stmt paramMap varMap
            case newStmtTable of
                Left err        -> Left err
                Right stmtTable -> Right $ (stmtTable):subStatements

checkStatement :: Identifier -> Statement -> ParameterMap -> VariableMap -> Either (IO Task) StatementTable
checkStatement procName stmt paramMap varMap =
    case stmt of
        Write expr -> checkWriteStmt procName expr paramMap varMap
        Read var -> do
            let eitherReadStatement = checkReadStmt procName var paramMap varMap
            case eitherReadStatement of
                Left err        -> Left err
                Right exprTable -> Right $ ReadTable exprTable
        Assign var expression -> do
          -- Assignment statement, e.g. a := 1
          let eitherVariableTable = checkVariable procName var paramMap varMap
          case eitherVariableTable of
              Left err            -> Left err
              Right variableTable -> do
                  checkAssignExpr procName expression variableTable paramMap varMap
        If expr stmts -> do
          let exprEither = checkConsition procName expr paramMap varMap
          case exprEither of
            Left err -> Left err
            Right exprTable -> do
              let stmtTablesEither = insertStatementList procName stmts paramMap varMap
              case stmtTablesEither of
                Left err         -> Left err
                Right stmtTables -> Right $ IfTable exprTable stmtTables
        IfElse expr stmts1 stmts2 -> do
          let exprEither = checkConsition procName expr paramMap varMap
          case exprEither of
            Left err -> Left err
            Right exprTable -> do
              let stmtTablesEither1 = insertStatementList procName stmts1 paramMap varMap
              case stmtTablesEither1 of
                Left err -> Left err
                Right stmtTables1 -> do
                  let stmtTablesEither2 = insertStatementList procName stmts2 paramMap varMap
                  case stmtTablesEither2 of
                    Left err -> Left err
                    Right stmtTables2 -> do
                      Right $ IfElseTable exprTable stmtTables1 stmtTables2
        While expr stmts -> do
          let exprEither = checkConsition procName expr paramMap varMap
          case exprEither of
            Left err -> Left err
            Right exprTable -> do
              let stmtTablesEither = insertStatementList procName stmts paramMap varMap
              case stmtTablesEither of
                Left err         -> Left err
                Right stmtTables -> Right $ WhileTable exprTable stmtTables
        -- TODO
        -- Call ident expr

checkWriteStmt :: Identifier -> Expression -> ParameterMap -> VariableMap -> Either (IO Task) StatementTable
checkWriteStmt procName expr paramMap varMap = do
    let eitherExpressionTable = checkExpression procName expr paramMap varMap
    case eitherExpressionTable of
      Left err        -> Left err
      Right exprTable -> Right $ WriteTable exprTable

checkReadStmt :: Identifier -> Variable -> ParameterMap -> VariableMap -> Either (IO Task) ExpressionTable
checkReadStmt procName var paramMap varMap = do
    let eitherExpressionTable = checkVariable procName var paramMap varMap
    case eitherExpressionTable of
        Left err        -> Left err
        Right exprTable -> Right $ exprTable

checkAssignExpr ::
  Identifier -> Expression -> ExpressionTable -> ParameterMap -> VariableMap -> Either (IO Task) StatementTable
checkAssignExpr procName expr variableTable paramMap varMap = do
    let eitherExpressionTable = checkExpression procName expr paramMap varMap
    case eitherExpressionTable of
      Left err        -> Left err
      Right expressionTable -> do
        let exprType = getAssignBaseType expressionTable
        checkAssignType procName variableTable expressionTable exprType

checkConsition :: Identifier -> Expression -> ParameterMap -> VariableMap -> Either (IO Task) ExpressionTable
checkConsition procName expr paramMap varMap = do
  let eitherExpressionTable = checkExpression procName expr paramMap varMap
      errorExit = exitWithConditionTypeError procName
  case eitherExpressionTable of
    Left err -> Left err
    Right exprTable -> do
      case exprTable of
        BoolTable _      -> Right exprTable
        OrTable _ _ _    -> Right exprTable
        AndTable _ _ _   -> Right exprTable
        NotTable _ _     -> Right exprTable
        EqTable _ _ _    -> Right exprTable
        NotEqTable _ _ _ -> Right exprTable
        LesTable _ _ _   -> Right exprTable
        LesEqTable _ _ _ -> Right exprTable
        GrtTable _ _ _   -> Right exprTable
        GrtEqTable _ _ _ -> Right exprTable
        otherwise        -> Left errorExit

checkExpression :: Identifier -> Expression -> ParameterMap -> VariableMap -> Either (IO Task) ExpressionTable
checkExpression procName expr paramMap varMap = do
    case expr of
      BoolConst  val -> Right $ BoolTable val
      IntConst   val -> Right $ IntTable val
      FloatConst val -> Right $ FloatTable val
      StrConst   val -> Right $ StringTable val
      ExprVar    val -> do
          let eitherVariableExpression = checkVariable procName val paramMap varMap
          case eitherVariableExpression of
              Left err                 -> Left err
              Right variableExpression -> Right variableExpression
      Add lExpr rExpr -> do
          let eitherAddExpression = checkOperationExpression procName addSymbol lExpr rExpr paramMap varMap
          case eitherAddExpression of
              Left err            -> Left err
              Right addExpression -> Right addExpression
      Mul lExpr rExpr -> do
          let eitherMulExpression = checkOperationExpression procName timesSymbol lExpr rExpr paramMap varMap
          case eitherMulExpression of
              Left err            -> Left err
              Right mulExpression -> Right mulExpression
      Sub lExpr rExpr -> do
          let eitherSubExpression = checkOperationExpression procName minusSymbol lExpr rExpr paramMap varMap
          case eitherSubExpression of
              Left err            -> Left err
              Right subExpression -> Right subExpression
      Div lExpr rExpr -> do
          let eitherDivExpression = checkOperationExpression procName divSymbol lExpr rExpr paramMap varMap
          case eitherDivExpression of
              Left err            -> Left err
              Right divExpression -> Right divExpression
      Or    lExpr rExpr ->
        checkLogicExpression   procName "||" lExpr rExpr paramMap varMap
      And   lExpr rExpr ->
        checkLogicExpression   procName "&&" lExpr rExpr paramMap varMap
      Eq    lExpr rExpr ->
        checkCompareExpression procName "="  lExpr rExpr paramMap varMap
      NotEq lExpr rExpr ->
        checkCompareExpression procName "!=" lExpr rExpr paramMap varMap
      Les   lExpr rExpr ->
        checkCompareExpression procName "<"  lExpr rExpr paramMap varMap
      LesEq lExpr rExpr ->
        checkCompareExpression procName "<=" lExpr rExpr paramMap varMap
      Grt   lExpr rExpr ->
        checkCompareExpression procName ">"  lExpr rExpr paramMap varMap
      GrtEq lExpr rExpr ->
        checkCompareExpression procName ">=" lExpr rExpr paramMap varMap
      UnaryMinus  expr  -> checkUnaryMinus procName expr paramMap varMap
      UnaryNot    expr  -> checkUnaryNot   procName expr paramMap varMap

checkLogicExpression ::
  Identifier -> String -> Expression -> Expression -> ParameterMap -> VariableMap -> Either (IO Task) ExpressionTable
checkLogicExpression procName operator lExpr rExpr paramMap varMap = do
  let lExprTableEither = checkLogicSubExpression procName operator lExpr paramMap varMap
  case lExprTableEither of
    Left err -> Left err
    Right lExprTable -> do
      let rExprTableEither = checkLogicSubExpression procName operator rExpr paramMap varMap
      case rExprTableEither of
        Left err -> Left err
        Right rExprTable -> do
          case operator of
            "||" -> Right $ OrTable lExprTable rExprTable BoolType
            "&&" -> Right $ AndTable lExprTable rExprTable BoolType

checkLogicSubExpression ::
  Identifier -> String -> Expression -> ParameterMap -> VariableMap -> Either (IO Task) ExpressionTable
checkLogicSubExpression procName operator expr paramMap varMap = do
  let exprTableEither = checkExpression procName expr paramMap varMap
      errorExit = exitWithLogicExprTypeError procName operator
  case exprTableEither of
    Left err -> Left err
    Right exprTable -> do
      case exprTable of
        VariableTable _ exprType -> checkBoolType exprTable exprType errorExit
        BoolTable  _             -> Right exprTable
        OrTable    _ _ exprType  -> checkBoolType exprTable exprType errorExit
        AndTable   _ _ exprType  -> checkBoolType exprTable exprType errorExit
        EqTable    _ _ exprType  -> checkBoolType exprTable exprType errorExit
        NotEqTable _ _ exprType  -> checkBoolType exprTable exprType errorExit
        LesTable   _ _ exprType  -> checkBoolType exprTable exprType errorExit
        LesEqTable _ _ exprType  -> checkBoolType exprTable exprType errorExit
        GrtTable   _ _ exprType  -> checkBoolType exprTable exprType errorExit
        GrtEqTable _ _ exprType  -> checkBoolType exprTable exprType errorExit
        NotTable   _ exprType    -> checkBoolType exprTable exprType errorExit
        otherwise -> Left errorExit

checkUnaryNot ::
  Identifier -> Expression -> ParameterMap -> VariableMap -> Either (IO Task) ExpressionTable
checkUnaryNot procName expr paramMap varMap = do
  let eitherUnaryNotExpression = checkLogicSubExpression procName "!" expr paramMap varMap
  case eitherUnaryNotExpression of
    Left err                 -> Left err
    Right unaryNotExpression -> Right (NotTable unaryNotExpression BoolType)

checkCompareExpression ::
  Identifier -> String -> Expression -> Expression -> ParameterMap -> VariableMap -> Either (IO Task) ExpressionTable
checkCompareExpression procName operator lExpr rExpr paramMap varMap = do
  let lExprTableEither = checkCompareSubExpression procName operator lExpr paramMap varMap
  case lExprTableEither of
    Left err -> Left err
    Right lExprTable -> do
      let rExprTableEither = checkCompareSubExpression procName operator rExpr paramMap varMap
      case rExprTableEither of
        Left err -> Left err
        Right rExprTable -> do
          case operator of
            "="  -> checkSameType procName operator lExprTable rExprTable
            "!=" -> checkSameType procName operator lExprTable rExprTable
            "<"  -> checkBaseType procName operator lExprTable rExprTable
            "<=" -> checkBaseType procName operator lExprTable rExprTable
            ">"  -> checkBaseType procName operator lExprTable rExprTable
            ">=" -> checkBaseType procName operator lExprTable rExprTable

checkCompareSubExpression ::
  Identifier -> String -> Expression -> ParameterMap -> VariableMap -> Either (IO Task) ExpressionTable
checkCompareSubExpression procName operator expr paramMap varMap = do
  let exprTableEither = checkExpression procName expr paramMap varMap
      errorExit = exitWithComparisonTypeError procName operator
  case exprTableEither of
    Left err -> Left err
    Right exprTable -> do
      case exprTable of
       StringTable _ -> Left errorExit
       otherwise     -> Right exprTable

checkUnaryMinus ::
  Identifier -> Expression -> ParameterMap -> VariableMap -> Either (IO Task) ExpressionTable
checkUnaryMinus procName expr paramMap varMap = do
  let exprTableEither = checkExpression procName expr paramMap varMap
  case exprTableEither of
    Left err -> Left err
    Right exprTable -> do
      let exprTypeEither = checkUnaryMinusType procName exprTable
      case exprTypeEither of
        Left err -> Left err
        Right exprType -> Right $ NegativeTable exprTable exprType

checkUnaryMinusType ::
  Identifier -> ExpressionTable -> Either (IO Task) BaseType
checkUnaryMinusType procName exprTable = do
  let errorExit = exitWithUnaryMinusError procName
  case exprTable of
    VariableTable _   exprType -> checkNotBoolType exprType errorExit
    IntTable      _            -> Right IntType
    FloatTable    _            -> Right FloatType
    AddTable      _ _ exprType -> checkNotBoolType exprType errorExit
    SubTable      _ _ exprType -> checkNotBoolType exprType errorExit
    MulTable      _ _ exprType -> checkNotBoolType exprType errorExit
    DivTable      _ _ exprType -> checkNotBoolType exprType errorExit
    NegativeTable _   exprType -> checkNotBoolType exprType errorExit
    otherwise -> Left errorExit


checkBoolType ::
  ExpressionTable -> BaseType -> IO Task -> Either (IO Task) ExpressionTable
checkBoolType exprTable exprType errorExit =
  case exprType of
    BoolType  -> Right exprTable
    otherwise -> Left errorExit

checkNotBoolType :: BaseType -> IO Task -> Either (IO Task) BaseType
checkNotBoolType exprType errorExit =
  case exprType of
    BoolType  -> Left errorExit
    otherwise -> Right exprType

-------------------------------------------------------------------------------
-- Note: Please filter the StringTable before using this function
--       Cannot be used for String
-- Check whether the type of given ExpressionTables are the same
-------------------------------------------------------------------------------
checkSameType ::
  Identifier -> String -> ExpressionTable -> ExpressionTable ->
  Either (IO Task) ExpressionTable
checkSameType procName operator lExpr rExpr = do
  let lType = getBaseType lExpr
      rType = getBaseType rExpr
  if lType == rType
    then Right (getComparisonTable operator lExpr rExpr lType)
    else Left (exitWithNotSameTypeError procName operator)

checkBaseType ::
  Identifier -> String -> ExpressionTable -> ExpressionTable ->
  Either (IO Task) ExpressionTable
checkBaseType procName operator lExpr rExpr = do
  let lType = getBaseType lExpr
      rType = getBaseType rExpr
  if lType /= rType
    then do
      { let exprTypeEither = getExpressionTableType procName lExpr rExpr
      ; case exprTypeEither of
          Left err -> Left err
          Right exprType ->
            Right (getComparisonTable operator lExpr rExpr exprType)
      }
    else Right (getComparisonTable operator lExpr rExpr lType)

getComparisonTable ::
  String -> ExpressionTable -> ExpressionTable -> BaseType -> ExpressionTable
getComparisonTable operator lExpr rExpr baseType =
  case operator of
    "="  -> EqTable    lExpr rExpr baseType
    "!=" -> NotEqTable lExpr rExpr baseType
    "<"  -> LesTable   lExpr rExpr baseType
    "<=" -> LesEqTable lExpr rExpr baseType
    ">"  -> GrtTable   lExpr rExpr baseType
    ">=" -> GrtEqTable lExpr rExpr baseType

-------------------------------------------------------------------------------
-- Note: Please filter the StringTable before using this function
--       Cannot be used for String
-- Return the BaseType of given ExpressionTable
-------------------------------------------------------------------------------
getBaseType :: ExpressionTable -> BaseType
getBaseType exprTable =
  case exprTable of
    VariableTable _ exprType  -> exprType
    BoolTable  _              -> BoolType
    IntTable   _              -> IntType
    FloatTable _              -> FloatType
    AddTable   _ _  exprType  -> exprType
    SubTable   _ _  exprType  -> exprType
    MulTable   _ _  exprType  -> exprType
    DivTable   _ _  exprType  -> exprType
    OrTable    _ _  exprType  -> exprType
    AndTable   _ _  exprType  -> exprType
    EqTable    _ _  exprType  -> exprType
    NotEqTable _ _  exprType  -> exprType
    LesTable   _ _  exprType  -> exprType
    LesEqTable _ _  exprType  -> exprType
    GrtTable   _ _  exprType  -> exprType
    GrtEqTable _ _  exprType  -> exprType
    NegativeTable _ exprType  -> exprType
    NotTable      _ exprType  -> exprType

checkAssignType ::
  Identifier -> ExpressionTable -> ExpressionTable -> BaseType -> Either (IO Task) StatementTable
checkAssignType procName variableTable expressionTable exprType = do
    let varType = variableType variableTable
        varName = varId $ variable variableTable
    if varType == exprType
        then Right $ AssignTable variableTable expressionTable
        else if (FloatType == varType) && (IntType == exprType)
            then Right $ AssignTable variableTable expressionTable
            else Left $ exitWithAssignTypeError procName varName

checkVariable :: Identifier -> Variable -> ParameterMap -> VariableMap -> Either (IO Task) ExpressionTable
checkVariable procName var paramMap varMap = do
    case (M.member id paramMap) of
        True -> Right $ VariableTable var paramBaseType
        False -> do
            case (M.member id varMap) of
                True  -> Right $ VariableTable var varBaseType
                False -> Left $ exitWithUndefinedVariable id
            where varBaseType = lookupBaseTypeVarMap id varMap
    where id = varId var
          paramBaseType = lookupBaseTypeParamMap id paramMap

checkOperationExpression :: Identifier -> String -> Expression -> Expression -> ParameterMap -> VariableMap -> Either (IO Task) ExpressionTable
checkOperationExpression procName operator lExpr rExpr paramMap varMap = do
    let leftExprTableEither = checkExpression procName lExpr paramMap varMap
    case leftExprTableEither of
        Left err -> Left err
        Right leftExprTable -> do
            let rightExprTableEither = checkExpression procName rExpr paramMap varMap
            case rightExprTableEither of
                 Left err -> Left err
                 Right rightExprTable -> do
                     let baseTypeEither = getExpressionTableType procName leftExprTable rightExprTable
                     case baseTypeEither of
                         Left err -> Left err
                         Right baseType -> Right $ insertExpressionTableByOperator operator leftExprTable rightExprTable baseType

insertExpressionTableByOperator :: String -> ExpressionTable -> ExpressionTable -> BaseType -> ExpressionTable
insertExpressionTableByOperator operator lExpressionTable rExpressionTable baseType
  | operator == addSymbol   = AddTable lExpressionTable rExpressionTable baseType
  | operator == minusSymbol = SubTable lExpressionTable rExpressionTable baseType
  | operator == timesSymbol = MulTable lExpressionTable rExpressionTable baseType
  | operator == divSymbol   = DivTable lExpressionTable rExpressionTable baseType

getExpressionTableType :: Identifier -> ExpressionTable -> ExpressionTable -> Either (IO Task) BaseType
getExpressionTableType procName leftExprTable rightExprTable = do
    case leftExprTable of
       IntTable _ -> do
         case rightExprTable of
           IntTable _               -> Right IntType
           FloatTable _             -> Right FloatType
           VariableTable _ rVarType -> Right rVarType
           AddTable _ _ rAddType    -> Right rAddType
           SubTable _ _ rSubType    -> Right rSubType
           MulTable _ _ rMulType    -> Right rMulType
           DivTable _ _ rDivType    -> Right rDivType
           NegativeTable _ rVarType -> Right rVarType
           otherwise                -> Left $ exitWithTypeError procName
       FloatTable _ -> Right FloatType
       VariableTable _ lVarType -> getSubExpressionTableType procName lVarType rightExprTable
       AddTable    _ _ lAddType -> getSubExpressionTableType procName lAddType rightExprTable
       SubTable    _ _ lSubType -> getSubExpressionTableType procName lSubType rightExprTable
       MulTable    _ _ lMulType -> getSubExpressionTableType procName lMulType rightExprTable
       DivTable    _ _ lDivType -> getSubExpressionTableType procName lDivType rightExprTable
       NegativeTable _ lVarType -> getSubExpressionTableType procName lVarType rightExprTable
       otherwise                -> Left $ exitWithTypeError procName

getSubExpressionTableType :: Identifier -> BaseType -> ExpressionTable -> Either (IO Task) BaseType
getSubExpressionTableType procName baseType expressionTable = do
 case expressionTable of
   IntTable _               -> Right baseType
   FloatTable _             -> Right FloatType
   VariableTable _ rVarType -> chooseType procName baseType rVarType
   AddTable _ _ rAddType    -> chooseType procName baseType rAddType
   SubTable _ _ rSubType    -> chooseType procName baseType rSubType
   MulTable _ _ rMulType    -> chooseType procName baseType rMulType
   DivTable _ _ rDivType    -> chooseType procName baseType rDivType
   NegativeTable _ rVarType -> chooseType procName baseType rVarType
   otherwise                -> Left $ exitWithTypeError procName

chooseType :: Identifier -> BaseType -> BaseType -> Either (IO Task) BaseType
chooseType _ IntType IntType   = Right IntType
chooseType _ FloatType _       = Right FloatType
chooseType _ _ FloatType       = Right FloatType
chooseType procName BoolType _ = Left $ exitWithTypeError procName
chooseType procName _ BoolType = Left $ exitWithTypeError procName

-------------------------------------------------------------------------------
---- Check whether the main procedure is parameter-less.
---------------------------------------------------------------------------------
checkMainParam :: [Parameter] -> IO Task
checkMainParam [] = return Unit
checkMainParam _  = do
  exitWithError "'main()' procedure should be parameter-less." MainWithParam

-------------------------------------------------------------------------------
-- Check the number of main procedure.
-------------------------------------------------------------------------------
checkMainNum :: Int -> IO Task
checkMainNum numMain
  | 0 == numMain = do
      exitWithError "There is no 'main()' procedure." MissingMain
  | 1 == numMain = return Unit
  | otherwise = do
      exitWithError "There is more than one 'main()' procedure" MultipleMain

-------------------------------------------------------------------------------
-- Get the list of main procedure.
-------------------------------------------------------------------------------
getMainProcedureList :: [Procedure] -> [Procedure]
getMainProcedureList [] = []
getMainProcedureList (proc:procs)
  | "main" == (getProcedureIdentifier proc) = proc : getMainProcedureList procs
  | otherwise = getMainProcedureList procs

checkMainProc :: GoatProgram -> IO ()
checkMainProc program = do
  { let mainList = getMainProcedureList $ procedures program
  ; checkMainNum $ length $ mainList
  ; checkMainParam $ parameters $ header $ head mainList
  ; return ()
  }

-------------------------------------------------------------------------------
-- Main entry of semantic Analyze module.
-------------------------------------------------------------------------------
semanticAnalyse :: GoatProgram -> Either (IO Task) ProgramMap
semanticAnalyse program = insertProcList (procedures program) M.empty
-- semanticAnalyse program = insertProcList $ procedures program
