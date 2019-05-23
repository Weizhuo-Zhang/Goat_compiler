module Analyze where

import GoatAST
import GoatExit
import qualified Data.Map.Strict as M
import SymbolTable

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

getIfConditionTypeErrorMessage :: Identifier -> String
getIfConditionTypeErrorMessage procName =
  "If condition type error! The type must be bool. In procedure " ++
  "\"" ++ procName ++ "\""

exitWithIfConditionTypeError :: Identifier -> IO Task
exitWithIfConditionTypeError procName =
  exitWithError
  (getIfConditionTypeErrorMessage procName)
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
    Left err -> Left err
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
      let varMap = insertVariableMap procedureName (bodyVarDeclarations procedureBody) subParamMap M.empty
      case varMap of
        Left err -> Left err
        Right subVarMap -> do
          let newStatements = insertStatementList procedureName (bodyStatements procedureBody)
          case newStatements of
            Left err -> Left err
            Right subStatements ->
              Right ( ProcedureTable
                      subParamMap
                      subVarMap
                      subStatements
                    )
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

insertStatementList ::
  Identifier -> [Statement] -> Either (IO Task) [StatementTable]
insertStatementList procName (stmt:[]) = do
  let newStmtTable = checkStatement procName stmt
  case newStmtTable of
    Left err -> Left err
    Right stmtTable -> Right $ (stmtTable):[]
insertStatementList procName (stmt:stmts) = do
  let newStatements = insertStatementList procName stmts
  case newStatements of
    Left err            -> Left err
    Right subStatements -> do
      let newStmtTable = checkStatement procName stmt
      case newStmtTable of
        Left err -> Left err
        Right stmtTable -> Right $
          (stmtTable):subStatements

checkStatement :: Identifier -> Statement -> Either (IO Task) StatementTable
checkStatement procName stmt = do
  case stmt of
    Write expr -> do
      let exprEither = checkWriteStmt procName expr
      case exprEither of
        Left err -> Left err
        Right exprTable -> Right (WriteTable exprTable)
    If expr stmts -> do
      let exprEither = checkIfConsition procName expr
          stmtTablesEither = insertStatementList procName stmts
      case exprEither of
        Left err -> Left err
        Right exprTable -> do
          case stmtTablesEither of
            Left err -> Left err
            Right stmtTables -> Right (IfTable exprTable stmtTables)
--        _ -> undefined


-- TODO: Complete the type matching in write statement
checkWriteStmt :: Identifier -> Expression -> Either (IO Task) ExpressionTable
checkWriteStmt procName expr = do
  let newExprTable = checkExpression procName expr
  case newExprTable of
    Left err -> Left err
    Right exprTable -> Right $ exprTable

checkIfConsition :: Identifier -> Expression -> Either (IO Task) ExpressionTable
checkIfConsition procName expr = do
  let newExprTable = checkExpression procName expr
      errorExit = exitWithIfConditionTypeError procName
  case newExprTable of
    Left err -> Left err
    Right exprTable -> do
      case exprTable of
        BoolTable val -> Right exprTable
        SingleExprTable expr exprType -> do
          case exprType of
            BoolType -> Right exprTable
            _    -> Left errorExit
        OrTable lExpr rExpr exprType -> Right exprTable
        AndTable lExpr rExpr exprType -> Right exprTable
--        DoubleExprTable lExpr rExpr exprType -> do
--          case exprType of
--            BoolType -> Right exprTable
--            _    -> Left errorExit
        _ -> Left errorExit

checkExpression :: Identifier -> Expression -> Either (IO Task) ExpressionTable
checkExpression procName expr = do
  case expr of
    BoolConst  val         -> Right (BoolTable val)
    IntConst   val         -> Right (IntTable val)
    FloatConst val         -> Right (FloatTable val)
    StrConst   val         -> Right (StringTable val)
    Or    lExpr rExpr      -> do
        let exprTableEither = checkLogicExpression procName "||" lExpr rExpr
        case exprTableEither of
            Left err -> Left err
            Right exprTable -> Right exprTable
    And   lExpr rExpr      -> do
        let exprTableEither = checkLogicExpression procName "&&" lExpr rExpr
        case exprTableEither of
            Left err -> Left err
            Right exprTable -> Right exprTable
-- TODO
--    Eq    lExpr rExpr      ->
--    NotEq lExpr rExpr      ->
--    Les   lExpr rExpr      ->
--    LesEq lExpr rExpr      ->
--    Grt   lExpr rExpr      ->
--    GrtEq lExpr rExpr      ->
--    UnaryNot    expression ->

checkLogicExpression ::
  Identifier -> String -> Expression -> Expression -> Either (IO Task) ExpressionTable
checkLogicExpression procName operator lExpr rExpr = do
  let lExprTableEither = checkLogicSubExpression procName operator lExpr
  case lExprTableEither of
    Left err -> Left err
    Right lExprTable -> do
      let rExprTableEither = checkLogicSubExpression procName operator rExpr
      case rExprTableEither of
        Left err -> Left err
        Right rExprTable -> do
          case operator of
            "||" -> Right (OrTable lExprTable rExprTable BoolType)
            "&&" -> Right (AndTable lExprTable rExprTable BoolType)

checkLogicSubExpression ::
  Identifier -> String -> Expression -> Either (IO Task) ExpressionTable
checkLogicSubExpression procName operator expr = do
  let exprTableEither = checkExpression procName expr
      errorExit = exitWithLogicExprTypeError procName operator
  case exprTableEither of
    Left err -> Left err
    Right exprTable -> do
      case exprTable of
        BoolTable _ -> Right exprTable
        SingleExprTable expr exprType -> do
          case exprType of
            BoolType -> Right exprTable
            _    -> Left errorExit
        _ -> Left errorExit

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
