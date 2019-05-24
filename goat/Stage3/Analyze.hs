module Analyze where

import GoatAST
import GoatExit
import qualified Data.Map.Strict as M
import SymbolTable
import Data.Maybe

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
      case (M.lookup varName paramMap) of
        Just variable -> passingType variable

-------------------------------------------------------------------------------
-- lookup variable Map, It must have a base type
-------------------------------------------------------------------------------
lookupBaseTypeVarMap :: Identifier -> M.Map Identifier VariableDeclaration -> BaseType
lookupBaseTypeVarMap varName varMap =
      case M.lookup varName varMap of
        Just variable -> declarationType variable

-------------------------------------------------------------------------------
-- Get variable id
-------------------------------------------------------------------------------
getVariableId :: Variable -> Identifier
getVariableId = varId

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
              let varMap =
                      insertVariableMap
                          procedureName (bodyVarDeclarations procedureBody) subParamMap M.empty
              case varMap of
                  Left err -> Left err
                  Right subVarMap -> do
                      let newStatements = insertStatementList (bodyStatements procedureBody) subParamMap subVarMap
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

insertStatementList :: [Statement] -> ParameterMap -> VariableMap -> Either (IO Task) [StatementTable]
insertStatementList (stmt:[]) paramMap varMap = do
    let newStmtTable = checkStatement stmt paramMap varMap
    case newStmtTable of
        Left err -> Left err
        Right stmtTable -> Right $ (stmtTable):[]
insertStatementList (stmt:stmts) paramMap varMap = do
    let newStatements = insertStatementList stmts paramMap varMap
    case newStatements of
        Left err            -> Left err
        Right subStatements -> do
            let newStmtTable = checkStatement stmt paramMap varMap
            case newStmtTable of
                Left err -> Left err
                Right stmtTable -> Right $
                        (stmtTable):subStatements

checkStatement :: Statement -> ParameterMap -> VariableMap -> Either (IO Task) StatementTable
checkStatement stmt paramMap varMap =
    case stmt of
        Write expr -> do
            let newExpr = checkWriteStmt expr paramMap varMap
            case newExpr of
                Left err -> Left err
                Right exprTable -> Right (StatementTable stmt exprTable)
        Read var -> do
            let newExpr = checkReadStmt var paramMap varMap
            case newExpr of
                Left err -> Left err
                Right exprTable -> Right (StatementTable stmt exprTable)
--        _ -> undefined

checkWriteStmt :: Expression -> ParameterMap -> VariableMap -> Either (IO Task) ExpressionTable
checkWriteStmt expr paramMap varMap =
    case expr of
      StrConst val -> Right (StringTable val)
      IntConst val -> Right (IntTable val)
      FloatConst val -> Right (FloatTable val)
      BoolConst val -> Right (BoolTable val)
      ExprVar val -> checkVariable val paramMap varMap

checkReadStmt :: Variable -> ParameterMap -> VariableMap -> Either (IO Task) ExpressionTable
checkReadStmt var paramMap varMap =
    case (checkVariable var paramMap varMap) of
      Right exprTable -> Right exprTable
      Left err -> Left err


checkVariable :: Variable -> ParameterMap -> VariableMap -> Either (IO Task) ExpressionTable
checkVariable var paramMap varMap = do
  case (M.member id paramMap) of
      True -> Right (ExpressionTable (ExprVar var) paramBaseType)
      False -> do
          case (M.member id varMap) of
              True -> Right (ExpressionTable (ExprVar var) varBaseType)
              False -> Left $ exitWithUndefinedVariable id
          where id = varId var
                varBaseType = lookupBaseTypeVarMap id varMap
  where id = varId var
        paramBaseType = lookupBaseTypeParamMap id paramMap

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
