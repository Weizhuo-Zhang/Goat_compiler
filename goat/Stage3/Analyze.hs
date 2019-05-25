module Analyze where

import qualified Data.Map.Strict as M
import           Data.Maybe
import           GoatAST
import           GoatExit
import           SymbolTable

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

exitWithTypeError :: Identifier -> IO Task
exitWithTypeError procName =
  exitWithError ("There is a Type Error in the Statment in proc: " ++
                "\"" ++ procName ++ "\"") UnmatchedType
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
                Left err -> Left err
                Right stmtTable -> Right $
                        (stmtTable):subStatements

checkStatement :: Identifier -> Statement -> ParameterMap -> VariableMap -> Either (IO Task) StatementTable
checkStatement procName stmt paramMap varMap =
    case stmt of
        Write expr -> do
            let newExpr = checkWriteStmt procName expr paramMap varMap
            case newExpr of
                Left err        -> Left err
                Right exprTable -> Right (StatementTable stmt exprTable)
        Read var -> do
            let newExpr = checkReadStmt procName var paramMap varMap
            case newExpr of
                Left err        -> Left err
                Right exprTable -> Right (StatementTable stmt exprTable)
        Assign var expression -> do
          -- Assignment statement, e.g. a := 1
          let newExpr = checkVariable procName var paramMap varMap
          case newExpr of
              Left err        -> Left err
              Right exprTable -> Right (StatementTable stmt exprTable)
--        _ -> undefined

checkWriteStmt :: Identifier -> Expression -> ParameterMap -> VariableMap -> Either (IO Task) ExpressionTable
checkWriteStmt procName expr paramMap varMap = do
    let newExprTable = checkExpression procName expr paramMap varMap
    case newExprTable of
      Left err        -> Left err
      Right exprTable -> Right $ exprTable

checkReadStmt :: Identifier -> Variable -> ParameterMap -> VariableMap -> Either (IO Task) ExpressionTable
checkReadStmt procName var paramMap varMap = do
    let newExprTable = checkVariable procName var paramMap varMap
    case newExprTable of
      Right exprTable -> Right $ exprTable
      Left err        -> Left err

checkExpression :: Identifier -> Expression -> ParameterMap -> VariableMap -> Either (IO Task) ExpressionTable
checkExpression procName expr paramMap varMap = do
    case expr of
      BoolConst  val         -> Right (BoolTable val)
      IntConst   val         -> Right (IntTable val)
      FloatConst val         -> Right (FloatTable val)
      StrConst   val         -> Right (StringTable val)
      ExprVar    var -> checkVariable procName var paramMap varMap
      Add lExpr rExpr -> do
        let checkTable = checkOperationExpression procName "+" lExpr rExpr paramMap varMap
        case checkTable of
            Right exprTable -> Right $ exprTable
            Left err        -> Left err
      Sub lExpr rExpr -> do
        let checkTable = checkOperationExpression procName "-" lExpr rExpr paramMap varMap
        case checkTable of
            Right exprTable -> Right $ exprTable
            Left err        -> Left err
      Mul lExpr rExpr -> do
        let checkTable = checkOperationExpression procName "*" lExpr rExpr paramMap varMap
        case checkTable of
            Right exprTable -> Right $ exprTable
            Left err        -> Left err
      Div lExpr rExpr -> do
        let checkTable = checkOperationExpression procName "/" lExpr rExpr paramMap varMap
        case checkTable of
            Right exprTable -> Right $ exprTable
            Left err        -> Left err

checkVariable :: Identifier -> Variable -> ParameterMap -> VariableMap -> Either (IO Task) ExpressionTable
checkVariable procName var paramMap varMap = do
  case (M.member id paramMap) of
      True -> Right (VariableTable var paramBaseType)
      False -> do
          case (M.member id varMap) of
              True  -> Right (VariableTable var varBaseType)
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
insertExpressionTableByOperator operator lExpressionTable rExpressionTable baseType =
    case operator of
        "+" -> AddTable lExpressionTable rExpressionTable baseType
        "-" -> SubTable lExpressionTable rExpressionTable baseType
        "*" -> MulTable lExpressionTable rExpressionTable baseType
        "/" -> DivTable lExpressionTable rExpressionTable baseType

getExpressionTableType :: Identifier -> ExpressionTable -> ExpressionTable -> Either (IO Task) BaseType
getExpressionTableType procName leftExprTablt rightExprTable = do
    case leftExprTablt of
       IntTable _ -> do
         case rightExprTable of
           IntTable _               -> Right IntType
           FloatTable _             -> Right FloatType
           BoolTable _              -> Left $ exitWithTypeError procName
           VariableTable _ rVarType -> Right rVarType
           AddTable _ _ rAddType    -> Right rAddType
           SubTable _ _ rSubType    -> Right rSubType
           MulTable _ _ rMulType    -> Right rMulType
           DivTable _ _ rDivType    -> Right rDivType
       FloatTable _ -> Right FloatType
       BoolTable _ -> Left $ exitWithTypeError procName
       VariableTable _ lVarType -> getSubExpressionTableType procName lVarType rightExprTable
       AddTable _ _ lAddType -> getSubExpressionTableType procName lAddType rightExprTable
       SubTable _ _ lSubType -> getSubExpressionTableType procName lSubType rightExprTable
       MulTable _ _ lMulType -> getSubExpressionTableType procName lMulType rightExprTable
       DivTable _ _ lDivType -> getSubExpressionTableType procName lDivType rightExprTable

getSubExpressionTableType :: Identifier -> BaseType -> ExpressionTable -> Either (IO Task) BaseType
getSubExpressionTableType procName baseType expressionTable = do
 case expressionTable of
   IntTable _               -> Right baseType
   FloatTable _             -> Right FloatType
   BoolTable _              -> Left $ exitWithTypeError procName
   VariableTable _ rVarType -> chooseType procName baseType rVarType
   AddTable _ _ rAddType    -> chooseType procName baseType rAddType
   SubTable _ _ rSubType    -> chooseType procName baseType rSubType
   MulTable _ _ rMulType    -> chooseType procName baseType rMulType
   DivTable _ _ rDivType    -> chooseType procName baseType rDivType

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
