module Analyzer where

import           AnalyzerUtil
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

insertVariableMap ::
  Identifier -> [VariableDeclaration] -> ParameterMap -> VariableMap ->
  Either (IO Task) VariableMap
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

insertStatementList ::
  Identifier -> [Statement] -> ParameterMap -> VariableMap -> ProgramMap ->
  Either (IO Task) [StatementTable]
insertStatementList procName (stmt:[]) paramMap varMap procMap = do
  let newStmtTable = checkStatement procName stmt paramMap varMap procMap
  case newStmtTable of
    Left err        -> Left err
    Right stmtTable -> Right $ (stmtTable):[]
insertStatementList procName (stmt:stmts) paramMap varMap procMap = do
  let newStatements = insertStatementList procName stmts paramMap varMap procMap
  case newStatements of
    Left err            -> Left err
    Right subStatements -> do
      let newStmtTable = checkStatement procName stmt paramMap varMap procMap
      case newStmtTable of
        Left err        -> Left err
        Right stmtTable -> Right $ (stmtTable):subStatements

checkStatement ::
  Identifier -> Statement -> ParameterMap -> VariableMap -> ProgramMap ->
  Either (IO Task) StatementTable
checkStatement procName stmt paramMap varMap procMap =
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
      let exprEither = checkCondition procName expr paramMap varMap
      case exprEither of
        Left err -> Left err
        Right exprTable -> do
          let stmtTablesEither = insertStatementList procName stmts paramMap varMap procMap
          case stmtTablesEither of
            Left err         -> Left err
            Right stmtTables -> Right $ IfTable exprTable stmtTables
    IfElse expr stmts1 stmts2 -> do
      let exprEither = checkCondition procName expr paramMap varMap
      case exprEither of
        Left err -> Left err
        Right exprTable -> do
          let stmtTablesEither1 = insertStatementList procName stmts1 paramMap varMap procMap
          case stmtTablesEither1 of
            Left err -> Left err
            Right stmtTables1 -> do
              let stmtTablesEither2 = insertStatementList procName stmts2 paramMap varMap procMap
              case stmtTablesEither2 of
                Left err -> Left err
                Right stmtTables2 -> do
                  Right $ IfElseTable exprTable stmtTables1 stmtTables2
    While expr stmts -> do
      let exprEither = checkCondition procName expr paramMap varMap
      case exprEither of
        Left err -> Left err
        Right exprTable -> do
          let stmtTablesEither = insertStatementList procName stmts paramMap varMap procMap
          case stmtTablesEither of
            Left err         -> Left err
            Right stmtTables -> Right $ WhileTable exprTable stmtTables
    Call procId argExprs -> do
      let exprsEither = checkCallStmt procName procId argExprs procMap paramMap varMap
      case exprsEither of
        Left err -> Left err
        Right (expreTables, params) -> Right $ CallTable procId expreTables params

checkArguments ::
  Identifier -> Identifier -> [Expression] -> [BaseType] -> [Parameter] ->
  ParameterMap -> VariableMap -> Either (IO Task) [ExpressionTable]
--checkArguments procName procId [e] [b] paramMap varMap = do
checkArguments procName procId (e:[]) (b:[]) (parameter:[]) paramMap varMap = do
  let paramIndicator  = (passingIndicator parameter)
      eitherExprTable =
            checkCallExpr procName procId paramIndicator e paramMap varMap
  case eitherExprTable of
    Left err -> Left err
    Right expressionTable -> do
      let exprBaseType = getExpressionBaseType expressionTable
      if (exprBaseType == b)
      then Right [expressionTable]
      else case paramIndicator of
             RefType -> Left $ exitWithCallParamLengthDiff procName procId
             varType -> if (FloatType == b && IntType == exprBaseType)
                            then Right [expressionTable]
                            else Left $ exitWithCallParamLengthDiff procName procId
checkArguments procName procId (e:es) (b:bs) (parameter:parameters) paramMap varMap = do
  let expressionTables = checkArguments procName procId es bs parameters paramMap varMap
  case expressionTables of
    Left err -> Left err
    Right exprTables -> do
      let paramIndicator  = (passingIndicator parameter)
          eitherExprTable =
                checkCallExpr procName procId paramIndicator e paramMap varMap
      case eitherExprTable of
        Left err -> Left err
        Right expressionTable -> do
          let exprBaseType = getExpressionBaseType expressionTable
          if (exprBaseType == b)
          then Right $ [expressionTable] ++ exprTables
          else case paramIndicator of
                 RefType -> Left $ exitWithCallParamLengthDiff procName procId
                 varType -> if (FloatType == b && IntType == exprBaseType)
                                then Right $ [expressionTable] ++ exprTables
                                else Left $ exitWithCallParamLengthDiff procName procId

checkCallExpr ::
  Identifier -> Identifier -> ParameterIndicator -> Expression ->
  ParameterMap -> VariableMap -> Either (IO Task) ExpressionTable
checkCallExpr procName procId paramIndicator expr paramMap varMap = do
  let eitherExprTable = checkExpression procName expr paramMap varMap
  case eitherExprTable of
    Left err -> Left err
    Right expressionTable -> do
      case paramIndicator of
        VarType -> Right expressionTable
        RefType -> do
          case expressionTable of
             VariableTable _ _ -> Right  expressionTable
             otherwise         -> Left $ exitWithInvalidCallExpr procName procId


paramMapToList :: ParameterMap -> [Parameter]
paramMapToList paramMap =
  [(snd param) | param <- paramNewList]
  where paramList = M.toList paramMap;
        paramOrderedMap = M.fromList [snd param | param <- paramList]
        paramNewList = M.toList paramOrderedMap

checkCallStmt ::
  Identifier -> Identifier -> [Expression] -> ProgramMap -> ParameterMap ->
  VariableMap -> Either (IO Task) ([ExpressionTable], [Parameter])
checkCallStmt procName calledProcId argExprs procMap paramMap varMap = do
  case M.lookup calledProcId procMap of
    Just calledProcTable -> do
      let paramList = paramMapToList $ parameterMap calledProcTable
          paramBaseTypes = [passingType param | param <- paramList]
      case ((length argExprs) == (length paramList)) of
        True -> do
          case ((length argExprs) == 0) of
            True -> Right ([], [])
            otherwise -> do
              let expreTables = checkArguments procName calledProcId argExprs paramBaseTypes paramList paramMap varMap
              case expreTables of
                Left err               -> Left err
                Right expressionTables -> Right (expressionTables, paramList)
        otherwise -> Left $ exitWithCallParamLengthDiff procName calledProcId
    Nothing -> Left $ exitWithProcNotFound procName calledProcId

checkWriteStmt ::
  Identifier -> Expression -> ParameterMap -> VariableMap ->
  Either (IO Task) StatementTable
checkWriteStmt procName expr paramMap varMap = do
  let eitherExpressionTable = checkExpression procName expr paramMap varMap
  case eitherExpressionTable of
    Left err        -> Left err
    Right exprTable -> Right $ WriteTable exprTable

checkReadStmt ::
  Identifier -> Variable -> ParameterMap -> VariableMap ->
  Either (IO Task) ExpressionTable
checkReadStmt procName var paramMap varMap = do
  let eitherExpressionTable = checkVariable procName var paramMap varMap
  case eitherExpressionTable of
    Left err        -> Left err
    Right exprTable -> Right $ exprTable

checkAssignExpr ::
  Identifier -> Expression -> ExpressionTable -> ParameterMap ->
  VariableMap -> Either (IO Task) StatementTable
checkAssignExpr procName expr variableTable paramMap varMap = do
  let eitherExpressionTable = checkExpression procName expr paramMap varMap
  case eitherExpressionTable of
    Left err        -> Left err
    Right expressionTable -> do
      let exprType = getExpressionBaseType expressionTable
      checkAssignType procName variableTable expressionTable exprType

checkCondition ::
  Identifier -> Expression -> ParameterMap -> VariableMap ->
  Either (IO Task) ExpressionTable
checkCondition procName expr paramMap varMap = do
  let eitherExpressionTable = checkExpression procName expr paramMap varMap
      errorExit = exitWithConditionTypeError procName
  case eitherExpressionTable of
    Left err -> Left err
    Right exprTable -> do
      case exprTable of
        VariableTable _ varType -> do
          case varType of
            BoolType  -> Right exprTable
            otherwise -> Left errorExit
        BoolTable  _     -> Right exprTable
        OrTable    _ _ _ -> Right exprTable
        AndTable   _ _ _ -> Right exprTable
        NotTable   _ _   -> Right exprTable
        EqTable    _ _ _ -> Right exprTable
        NotEqTable _ _ _ -> Right exprTable
        LesTable   _ _ _ -> Right exprTable
        LesEqTable _ _ _ -> Right exprTable
        GrtTable   _ _ _ -> Right exprTable
        GrtEqTable _ _ _ -> Right exprTable
        otherwise        -> Left errorExit

checkExpression ::
  Identifier -> Expression -> ParameterMap -> VariableMap ->
  Either (IO Task) ExpressionTable
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
      let eitherAddExpression = checkArthmeticOperation
                                procName
                                addSymbol
                                lExpr
                                rExpr
                                paramMap
                                varMap
      case eitherAddExpression of
          Left err            -> Left err
          Right addExpression -> Right addExpression
    Mul lExpr rExpr -> do
      let eitherMulExpression = checkArthmeticOperation
                                procName
                                timesSymbol
                                lExpr
                                rExpr
                                paramMap
                                varMap
      case eitherMulExpression of
          Left err            -> Left err
          Right mulExpression -> Right mulExpression
    Sub lExpr rExpr -> do
      let eitherSubExpression = checkArthmeticOperation
                                procName
                                minusSymbol
                                lExpr
                                rExpr
                                paramMap
                                varMap
      case eitherSubExpression of
          Left err            -> Left err
          Right subExpression -> Right subExpression
    Div lExpr rExpr -> do
      let eitherDivExpression = checkArthmeticOperation
                                procName
                                divSymbol
                                lExpr
                                rExpr
                                paramMap
                                varMap
      case eitherDivExpression of
          Left err            -> Left err
          Right divExpression -> Right divExpression
    Or    lExpr rExpr ->
      checkLogicExpression   procName "||" lExpr rExpr paramMap varMap
    And   lExpr rExpr ->
      checkLogicExpression   procName "&&" lExpr rExpr paramMap varMap
    Eq    lExpr rExpr ->
      checkCompareExpression procName equalSymbol lExpr rExpr paramMap varMap
    NotEq lExpr rExpr ->
      checkCompareExpression procName notEqualSymbol lExpr rExpr paramMap varMap
    Les   lExpr rExpr ->
      checkCompareExpression procName lessThanSymbol lExpr rExpr paramMap varMap
    LesEq lExpr rExpr ->
      checkCompareExpression procName lessThanOrEqualSymbol lExpr rExpr paramMap varMap
    Grt   lExpr rExpr ->
      checkCompareExpression procName greaterThanSymbol  lExpr rExpr paramMap varMap
    GrtEq lExpr rExpr ->
      checkCompareExpression procName greaterThanOrEqualSymbol lExpr rExpr paramMap varMap
    UnaryMinus  expr  -> checkUnaryMinus procName expr paramMap varMap
    UnaryNot    expr  -> checkUnaryNot   procName expr paramMap varMap

checkLogicExpression ::
  Identifier -> String -> Expression -> Expression -> ParameterMap ->
  VariableMap -> Either (IO Task) ExpressionTable
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
  Identifier -> String -> Expression -> ParameterMap -> VariableMap ->
  Either (IO Task) ExpressionTable
checkLogicSubExpression procName operator expr paramMap varMap = do
  let eitherExprTable = checkExpression procName expr paramMap varMap
  case eitherExprTable of
    Left err -> Left err
    Right exprTable -> do
      let errorExit = exitWithLogicExprTypeError procName operator
      case exprTable of
        VariableTable _ _ -> expressionIsBoolTyped exprTable errorExit
        BoolTable  _      -> Right exprTable
        OrTable    _ _ _  -> expressionIsBoolTyped exprTable errorExit
        AndTable   _ _ _  -> expressionIsBoolTyped exprTable errorExit
        EqTable    _ _ _  -> expressionIsBoolTyped exprTable errorExit
        NotEqTable _ _ _  -> expressionIsBoolTyped exprTable errorExit
        LesTable   _ _ _  -> expressionIsBoolTyped exprTable errorExit
        LesEqTable _ _ _  -> expressionIsBoolTyped exprTable errorExit
        GrtTable   _ _ _  -> expressionIsBoolTyped exprTable errorExit
        GrtEqTable _ _ _  -> expressionIsBoolTyped exprTable errorExit
        NotTable   _ _    -> expressionIsBoolTyped exprTable errorExit
        otherwise         -> Left errorExit

checkUnaryNot ::
  Identifier -> Expression -> ParameterMap -> VariableMap ->
  Either (IO Task) ExpressionTable
checkUnaryNot procName expr paramMap varMap = do
  let eitherUnaryNotExpression = checkLogicSubExpression procName "!" expr paramMap varMap
  case eitherUnaryNotExpression of
    Left err                 -> Left err
    Right unaryNotExpression -> Right (NotTable unaryNotExpression BoolType)

checkCompareExpression ::
  Identifier -> String -> Expression -> Expression -> ParameterMap ->
  VariableMap -> Either (IO Task) ExpressionTable
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
            "="  -> checkIfExpressionsAreSameType procName operator lExprTable rExprTable
            "!=" -> checkIfExpressionsAreSameType procName operator lExprTable rExprTable
            "<"  -> checkBaseType procName operator lExprTable rExprTable
            "<=" -> checkBaseType procName operator lExprTable rExprTable
            ">"  -> checkBaseType procName operator lExprTable rExprTable
            ">=" -> checkBaseType procName operator lExprTable rExprTable

checkCompareSubExpression ::
  Identifier -> String -> Expression -> ParameterMap -> VariableMap ->
  Either (IO Task) ExpressionTable
checkCompareSubExpression procName operator expr paramMap varMap = do
  let eitherExprTable = checkExpression procName expr paramMap varMap
      errorExit = exitWithComparisonTypeError procName operator
  case eitherExprTable of
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
        Left err       -> Left err
        Right exprType -> Right $ NegativeTable exprTable exprType

-------------------------------------------------------------------------------
-- Check if the expression  is BoolType, if so, return the expression type,
-- otherwise exit the program with provided error method.
-------------------------------------------------------------------------------
checkUnaryMinusType ::
  Identifier -> ExpressionTable -> Either (IO Task) BaseType
checkUnaryMinusType procName exprTable = do
  let errorExit = exitWithUnaryMinusError procName
  case exprTable of
    VariableTable _   exprType -> expressionTypeIsNotBool exprType errorExit
    IntTable      _            -> Right IntType
    FloatTable    _            -> Right FloatType
    AddTable      _ _ exprType -> expressionTypeIsNotBool exprType errorExit
    SubTable      _ _ exprType -> expressionTypeIsNotBool exprType errorExit
    MulTable      _ _ exprType -> expressionTypeIsNotBool exprType errorExit
    DivTable      _ _ exprType -> expressionTypeIsNotBool exprType errorExit
    NegativeTable _   exprType -> expressionTypeIsNotBool exprType errorExit
    otherwise                  -> Left errorExit

-------------------------------------------------------------------------------
-- Check if the expression type is BoolType, if so, return the expression type,
-- otherwise exit the program with provided error method.
-------------------------------------------------------------------------------
expressionIsBoolTyped ::
  ExpressionTable -> IO Task -> Either (IO Task) ExpressionTable
expressionIsBoolTyped exprTable errorExit = do
  let exprType = getExpressionBaseType exprTable
  case exprType of
    BoolType  -> Right exprTable
    otherwise -> Left errorExit

-------------------------------------------------------------------------------
-- Check if the expression type is BoolType, if so, exit the program with
-- error, otherwise return the expression type.
-------------------------------------------------------------------------------
expressionTypeIsNotBool :: BaseType -> IO Task -> Either (IO Task) BaseType
expressionTypeIsNotBool exprType errorExit =
  case exprType of
    BoolType  -> Left errorExit
    otherwise -> Right exprType

-------------------------------------------------------------------------------
-- Note: Please filter the StringTable before using this function
--       Cannot be used for String
-- Check whether the type of given ExpressionTables are the same
-------------------------------------------------------------------------------
checkIfExpressionsAreSameType ::
  Identifier -> String -> ExpressionTable -> ExpressionTable ->
  Either (IO Task) ExpressionTable
checkIfExpressionsAreSameType procName operator lExpr rExpr = do
  let lType = getExpressionTableBaseType lExpr
      rType = getExpressionTableBaseType rExpr
  if lType == rType
    then Right (getComparisonTable operator lExpr rExpr lType)
    else Left (exitWithNotSameTypeError procName operator)

checkBaseType ::
  Identifier -> String -> ExpressionTable -> ExpressionTable ->
  Either (IO Task) ExpressionTable
checkBaseType procName operator lExpr rExpr = do
  let lType = getExpressionTableBaseType lExpr
      rType = getExpressionTableBaseType rExpr
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
getComparisonTable operator lExpr rExpr baseType
  | operator == equalSymbol = EqTable    lExpr rExpr baseType
  | operator == notEqualSymbol = NotEqTable lExpr rExpr baseType
  | operator == lessThanSymbol = LesTable   lExpr rExpr baseType
  | operator == lessThanOrEqualSymbol = LesEqTable lExpr rExpr baseType
  | operator == greaterThanSymbol = GrtTable   lExpr rExpr baseType
  | operator == greaterThanOrEqualSymbol =GrtEqTable lExpr rExpr baseType

-------------------------------------------------------------------------------
-- Check if the provided variable's type matches the expression type, or if
-- the variable is float and expression is int, if so, return the assign
-- statement table, otherwise exit the program with assign type error.
-------------------------------------------------------------------------------
checkAssignType ::
  Identifier -> ExpressionTable -> ExpressionTable -> BaseType ->
  Either (IO Task) StatementTable
checkAssignType procName variableTable expressionTable exprType = do
    let varType      = variableType variableTable
        variableName = varName $ variable variableTable
    if varType == exprType
        then Right $ AssignTable variableTable expressionTable
        else if (FloatType == varType) && (IntType == exprType)
            then Right $ AssignTable variableTable expressionTable
            else Left $ exitWithAssignTypeError procName variableName

-------------------------------------------------------------------------------
-- Check if the provided variable is in the parameter map or the variable map,
-- if so, check the indicator and return the expression table, otherwise exit
-- the program with undefined variable error.
-------------------------------------------------------------------------------
checkVariable ::
  Identifier -> Variable -> ParameterMap -> VariableMap ->
  Either (IO Task) ExpressionTable
checkVariable procName var paramMap varMap = do
  let id = varId var
      paramBaseType = lookupBaseTypeParamMap id paramMap
  case (M.member id paramMap) of
    True  -> checkParamIndicator procName var paramBaseType
    False -> do
      let varDecl     = varMap M.! id
          varBaseType = lookupBaseTypeVarMap id varMap
      case (M.member id varMap) of
          True  -> checkVariableIndicator
                   procName
                   var
                   varDecl
                   varBaseType
                   paramMap
                   varMap
          False -> Left $ exitWithUndefinedVariable id

-------------------------------------------------------------------------------
-- Check if the provided variable is an array or matrix, if so, exit with the
-- variable indicator error, otherwise return the variable's expression table.
-------------------------------------------------------------------------------
checkParamIndicator ::
  Identifier -> Variable -> BaseType -> Either (IO Task) ExpressionTable
checkParamIndicator procName var paramBaseType = do
  let id = varId var
      varIndicator = varShapeIndicator var
      varSubTable  = VariableSubTable id NoIndicatorTable
  case varIndicator of
    NoIndicator -> Right $ VariableTable varSubTable paramBaseType
    otherwise   -> Left  $ exitWithVarIndicatorError procName id

-------------------------------------------------------------------------------
-- Check if the provided variable's shape indicator matches the provided
-- variable declaration shape indicator, if so, return the expression table,
-- otherwise exit the program with array matrix dimension typed error.
-------------------------------------------------------------------------------
checkVariableIndicator ::
  Identifier -> Variable -> VariableDeclaration -> BaseType -> ParameterMap ->
  VariableMap -> Either (IO Task) ExpressionTable
checkVariableIndicator procName var varDecl varBaseType paramMap varMap = do
  let id = varId var
      varIndicator  = varShapeIndicator var
      declarationVarIndicator = varShapeIndicator $ declarationVariable varDecl
      varSubTable   = VariableSubTable id NoIndicatorTable
  case (varIndicator, declarationVarIndicator)of
    (NoIndicator, NoIndicator) -> Right $ VariableTable varSubTable varBaseType
    (Array  n   , Array  _   ) -> checkArrayDimension
                                  procName
                                  n
                                  var
                                  varBaseType
                                  paramMap
                                  varMap
    (Matrix m n , Matrix _ _ ) -> checkMatrixDimensions
                                  procName
                                  (m, n)
                                  var
                                  varBaseType
                                  paramMap
                                  varMap
    otherwise -> Left  $ exitWithVarIndicatorNotSame procName id

-------------------------------------------------------------------------------
-- Check if the provided expression for array is IntType, if so, return the
-- expression table, otherwise exit the program with array matrix dimension
-- typed error.
-------------------------------------------------------------------------------
checkArrayDimension ::
  Identifier -> Expression -> Variable -> BaseType -> ParameterMap ->
  VariableMap -> Either (IO Task) ExpressionTable
checkArrayDimension procName expr var varBaseType paramMap varMap = do
  let varName = varId var
      eitherExpressionTable = checkDimension
                              procName
                              expr
                              varName
                              paramMap
                              varMap
  case eitherExpressionTable of
    Left  err             -> Left err
    Right expressionTable -> do
        let arrayTable = ArrayTable expressionTable
            varSubTable = VariableSubTable varName arrayTable
        Right $ VariableTable varSubTable varBaseType

-------------------------------------------------------------------------------
-- Check if the provided expressions are IntType, if so, return the expression
-- table, otherwise exit the program with array matrix dimension typed error.
-------------------------------------------------------------------------------
checkMatrixDimensions ::
  Identifier -> (Expression, Expression) -> Variable -> BaseType ->
  ParameterMap -> VariableMap -> Either (IO Task) ExpressionTable
checkMatrixDimensions procName (exprM, exprN) var varBaseType paramMap varMap = do
  -- check expression m first.
  let varName = varId var
      eitherExpressionTableM = checkDimension
                               procName
                               exprM
                               varName
                               paramMap
                               varMap
  case eitherExpressionTableM of
    Left  err             -> Left err
    Right expressionTableM -> do
      -- if express m passed, check expression n.
      let eitherExpressionTableN = checkDimension
                                   procName
                                   exprN
                                   varName
                                   paramMap
                                   varMap
      case eitherExpressionTableN of
        Left  err             -> Left err
        Right expressionTableN -> do
          let matrixTable = MatrixTable expressionTableM expressionTableN
              varSubTable = VariableSubTable varName matrixTable
          Right $ VariableTable varSubTable varBaseType

-------------------------------------------------------------------------------
-- Check if the given expression is IntType, if so, return the expression
-- table, otherwise exit the program with array matrix dimension typed error.
-------------------------------------------------------------------------------
checkDimension ::
  Identifier -> Expression -> String -> ParameterMap -> VariableMap ->
  Either (IO Task) ExpressionTable
checkDimension procName expr varName paramMap varMap = do
  let eitherExpressionTable = checkExpression procName expr paramMap varMap
  case eitherExpressionTable of
    Left  err             -> Left err
    Right expressionTable -> do
      let exprType = getExpressionBaseType expressionTable
      case exprType of
        IntType   -> Right expressionTable
        otherwise -> Left $ exitArrayMatrixDimensionTypeError procName varName

-------------------------------------------------------------------------------
-- Given the operator, left and right expression tables, and the base type,
-- return the expression table for the arthmetic operation.
-------------------------------------------------------------------------------
checkArthmeticOperation ::
  Identifier -> String -> Expression -> Expression -> ParameterMap ->
  VariableMap -> Either (IO Task) ExpressionTable
checkArthmeticOperation procName operator lExpr rExpr paramMap varMap = do
  let eitherLeftExprTable = checkExpression procName lExpr paramMap varMap
  case eitherLeftExprTable of
    Left err -> Left err
    Right leftExprTable -> do
      let eitherRightExprTable = checkExpression procName rExpr paramMap varMap
      case eitherRightExprTable of
        Left err -> Left err
        Right rightExprTable -> do
          let eitherBaseType = getExpressionTableType
                               procName
                               leftExprTable
                               rightExprTable
          case eitherBaseType of
            Left err -> Left err
            Right baseType -> Right $ insertExpressionTableByOperator
                                      operator
                                      leftExprTable
                                      rightExprTable
                                      baseType

-------------------------------------------------------------------------------
-- Given the operator, left and right expression tables, and the base type,
-- return the expression table.
-------------------------------------------------------------------------------
insertExpressionTableByOperator ::
  String -> ExpressionTable -> ExpressionTable -> BaseType -> ExpressionTable
insertExpressionTableByOperator operator lExprTable rExprTable baseType
  | operator == addSymbol   = AddTable lExprTable rExprTable baseType
  | operator == minusSymbol = SubTable lExprTable rExprTable baseType
  | operator == timesSymbol = MulTable lExprTable rExprTable baseType
  | operator == divSymbol   = DivTable lExprTable rExprTable baseType

-------------------------------------------------------------------------------
-- Insert statements tables into provided procedure' existing procedure table,
-- and return the new procedure table containing statements.
-------------------------------------------------------------------------------
insertStatementsIntoProcedureTable ::
  Procedure -> ProgramMap -> Either (IO Task) ProcedureTable
insertStatementsIntoProcedureTable procedure procMap =  do
  let eitherStatements = insertStatementList
                         procedureName
                         (bodyStatements $ body procedure)
                         paramMap
                         varMap
                         procMap
      procedureName = getProcedureIdentifier procedure
      procedureTable = procMap M.! procedureName
      paramMap = parameterMap procedureTable
      varMap = variableMap procedureTable
  case eitherStatements of
    Left err         -> Left err
    Right statements -> Right $ ProcedureTable paramMap varMap statements

-------------------------------------------------------------------------------
-- Insert statements tables into provided procedures' existing procedure
-- tables, and return the new procedure table containing statements.
-------------------------------------------------------------------------------
insertStatementsInProcList ::
  [Procedure] -> ProgramMap -> Either (IO Task) ProgramMap
insertStatementsInProcList (proc:[]) procMap = do
  let eitherProcedureTable = insertStatementsIntoProcedureTable proc procMap
      procName = getProcedureIdentifier proc
  case eitherProcedureTable of
    Left err             -> Left err
    Right procedureTable -> Right $ M.insert procName procedureTable procMap
insertStatementsInProcList (proc:procs) procMap = do
  let eitherProcedureTable = insertStatementsIntoProcedureTable proc procMap
      procName = getProcedureIdentifier proc
  case eitherProcedureTable of
    Left err -> Left err
    Right procedureTable -> do
      let eitherNewProcMap = insertStatementsInProcList procs procMap
      case eitherNewProcMap of
        Left err -> Left err
        Right newProcMap -> Right $ M.insert procName procedureTable newProcMap

-------------------------------------------------------------------------------
-- Analyze the given parameter list, insert them into a new parameter table and
-- return it.
-------------------------------------------------------------------------------
insertParameterMap ::
  Identifier -> [Parameter] -> Int -> ParameterMap ->
  Either (IO Task) ParameterMap
insertParameterMap _ [] _ paramMap = Right paramMap
insertParameterMap _ (param:[]) index paramMap =
  Right $ M.insert (passingIdent param) (index, param) paramMap
insertParameterMap procName (param:params) index paramMap = do
  let eitherParamMap = insertParameterMap procName params (index+1) paramMap
  case eitherParamMap of
    Left err -> Left err
    Right newParamMap -> do
      let paramName = (passingIdent param)
      case (M.member paramName newParamMap) of
        True  -> Left $ exitWithMultipleVarDeclaration paramName procName
        False -> Right $ M.insert paramName (index, param) newParamMap

-------------------------------------------------------------------------------
-- Analyze the given procedure, get the parameter and variable tables, and
-- insert them into a new procedure table, otherwise return error.
-------------------------------------------------------------------------------
insertProcedureTableWithoutStatements ::
  Procedure -> Either (IO Task) ProcedureTable
insertProcedureTableWithoutStatements procedure = do
  let  eitherParamMap = insertParameterMap
                        procedureName
                        (getProcedureParameters procedure)
                        0
                        M.empty
       procedureName = getProcedureIdentifier procedure
  case eitherParamMap of
    Left err -> Left err
    Right paramMap -> do
      let eitherVarMap = insertVariableMap
                         procedureName
                         (bodyVarDeclarations $ body procedure)
                         paramMap
                         M.empty
      case eitherVarMap of
        Left err     -> Left err
        Right varMap -> Right $ ProcedureTable paramMap varMap []

-------------------------------------------------------------------------------
-- Get the procedure table which does not contain procedure statements, and
-- insert it into the map storing program table, and return a new copy of the
-- map.
-------------------------------------------------------------------------------
insertProcWithoutStatements ::
  Procedure -> ProgramMap -> Either (IO Task) ProgramMap
insertProcWithoutStatements procedure procMap = do
  let eitherProcTable = insertProcedureTableWithoutStatements procedure
      procName = getProcedureIdentifier procedure
  case eitherProcTable of
    Left err        -> Left err
    Right procTable -> Right $ M.insert procName procTable procMap

-------------------------------------------------------------------------------
-- Get the procedure tables for provided procedure list, insert them into the
-- map provided, and return a new copy of the new map containing all
-- information.
-------------------------------------------------------------------------------
insertProcListWithoutStatements ::
  [Procedure] -> ProgramMap -> Either (IO Task) ProgramMap
insertProcListWithoutStatements (proc:[]) procMap =
  insertProcWithoutStatements proc procMap
insertProcListWithoutStatements (proc:procs) procMap = do
  let newProcMap = insertProcListWithoutStatements procs procMap
  case newProcMap of
    Left err -> Left err
    Right subProcMap -> do
      let procName = getProcedureIdentifier proc
      case (M.member procName subProcMap) of
        True  -> Left $ exitWithDuplicateProcedure procName
        False -> insertProcWithoutStatements proc subProcMap

-------------------------------------------------------------------------------
-- analyze procedure list.
-------------------------------------------------------------------------------
analyze :: [Procedure] -> Either (IO Task) ProgramMap
analyze procedures = do
  let eitherProgramMap = insertProcListWithoutStatements procedures M.empty
  case eitherProgramMap of
      Left err         -> Left err
      Right programMap -> insertStatementsInProcList procedures programMap

-------------------------------------------------------------------------------
-- Main entry of semantic Analyze module.
-------------------------------------------------------------------------------
semanticAnalyse :: GoatProgram -> Either (IO Task) ProgramMap
semanticAnalyse program = analyze $ procedures program
