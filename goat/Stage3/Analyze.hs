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
    let procTable =
            insertProcedureTable procName (parameters (header proc)) (body proc)
    case procTable of
        Left err -> Left err
        Right subProcTable -> Right $ M.insert procName subProcTable procMap
    where procName = (headerIdent $ header proc)
insertProcList (proc:procs) procMap = do
    let newProcMap = insertProcList procs procMap
    case newProcMap of
        Left err -> Left err
        Right subProcMap -> do
            let procName = headerIdent $ header proc
            case (M.member procName subProcMap) of
                True  -> Left $
                        exitWithError
                        ( "There are multiple procedures named " ++
                          "\"" ++ procName ++ "\"")
                        MultipleProc
                False -> do
                        let procTable =
                                insertProcedureTable
                                    procName (parameters (header proc)) (body proc)
                        case procTable of
                            Left err -> Left err
                            Right subProcTable ->
                                    Right $ M.insert procName subProcTable subProcMap

insertProcedureTable ::
    Identifier -> [Parameter] -> Body -> Either (IO Task) ProcedureTable
insertProcedureTable procName parameters body = do
    let paramMap = insertParameterMap procName parameters M.empty
    case paramMap of
        Left err -> Left err
        Right subParamMap -> Right (
                            ProcedureTable
                            subParamMap
                            (insertVariableMap $ bodyVarDeclarations body)
                            (bodyStatements body))

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
                True  -> Left $
                        exitWithError
                        ( "There are multiple variable declaration named " ++
                          "\"" ++ paramName ++ "\"" ++ " in procedure " ++
                          "\"" ++ procName ++ "\"")
                        MultipleVar
                False -> Right $
                        M.insert paramName param subParamMap

insertVariableMap :: [VariableDeclaration] -> VariableMap
insertVariableMap [] = M.empty
insertVariableMap (bodyVarDecl:[]) =
  M.insert (varId $ declarationVariable bodyVarDecl) bodyVarDecl M.empty
insertVariableMap (bodyVarDecl:bodyVarDecls) =
  M.insert (varId $ declarationVariable bodyVarDecl) bodyVarDecl (insertVariableMap bodyVarDecls)

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
countMain :: [Procedure] -> [Procedure]
countMain [] = []
countMain (proc:procs)
    | "main" == (headerIdent $ header proc) = proc : countMain procs
    | otherwise = countMain procs

checkMainProc :: GoatProgram -> IO ()
checkMainProc program = do
    { let mainList = countMain $ procedures program
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
