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
insertProcList (proc:[]) environment = Right newEnvironment
    where newEnvironment = M.insert
            (headerIdent $ header proc)
            (insertProcedureTable (header proc) (body proc))
            environment
insertProcList (proc:procs) environment = do
    let subEnvironment = insertProcList procs environment
    case subEnvironment of
        Left err -> Left err
        Right result -> do
            let newEnvironment = M.insert
                    (headerIdent $ header proc)
                    (insertProcedureTable (header proc) (body proc))
                    result
                procName = headerIdent $ header proc
            case (M.member procName result) of
                True ->
                    Left $ exitWithError
                                ("There are multiple procedures named " ++
                                 "\"" ++ procName ++ "\"")
                                MultipleProc
                False -> Right newEnvironment

-- insertProcList :: [Procedure] -> ProgramMap -> ProgramMap
-- insertProcList (proc:[]) environment = newEnvironment
--     where newEnvironment = M.insert
--             (headerIdent $ header proc)
--             (insertProcedureTable (header proc) (body proc))
--             environment
-- insertProcList (proc:procs) environment = newEnvironment
--     where newEnvironment = M.insert
--             (headerIdent $ header proc)
--             (insertProcedureTable (header proc) (body proc))
--             (insertProcList procs environment)

-- insertProcList :: [Procedure] -> ProgramMap
-- insertProcList (proc:[]) =
--   M.insert (headerIdent $ header proc) (insertProcedureTable (header proc) (body proc)) M.empty
-- insertProcList (proc:procs) =
--   M.insert (headerIdent $ header proc) (insertProcedureTable (header proc) (body proc)) (insertProcList procs)

insertProcedureTable :: Header -> Body -> ProcedureTable
insertProcedureTable header body =
  ProcedureTable (insertParameterMap $ parameters header) (insertVariableMap $ bodyVarDeclarations body) (bodyStatements body)

insertParameterMap :: [Parameter] -> ParameterMap
insertParameterMap [] = M.empty
insertParameterMap (param:[]) =
  M.insert (passingIdent param) param M.empty
insertParameterMap (param:params) =
  M.insert (passingIdent param) param (insertParameterMap params)

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
