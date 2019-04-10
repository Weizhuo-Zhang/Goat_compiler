module GoatPrettyPrint where

import GoatAST
import System.Exit(die)

-- TODO should trace line number
exitWithError :: String -> IO ()
exitWithError a = do
    die ("[ERROR] " ++ a)

printIndent :: Int -> IO ()
printIndent 0 = return ()
printIndent indent = do
    { putStr " "
    ; printIndent (indent - 1)
    }

printBaseType :: PType -> IO ()
printBaseType baseType = do
    case baseType of
        BoolType  -> putStr "bool"
        IntType   -> putStr "int"
        FloatType -> putStr "float"

printPassIndicator :: Pindicator -> IO ()
printPassIndicator pIndicator = do
    case pIndicator of
        VarType   -> putStr "var"
        RefType   -> putStr "ref"

printSIndicator :: () -> IO ()
printSIndicator () = return ()
-- printSIndicator

printParameters :: [Parameter] -> String -> IO ()
printParameters [] _                     = return ()
printParameters (param:params) seperator = do
    { putStr seperator
    ; printPassIndicator $ passingIndicator param
    ; putStr " "
    ; printBaseType $ passingType param
    ; putStr " "
    ; putStr $ passingIdent param
    ; printParameters (params) ", "
    }

printHeader :: Header -> IO ()
printHeader header = do
    { putStr   "proc "
    ; putStr $ headerIdent $ header
    ; putStr   " ("
    ; printParameters (parameters $ header) ""
    ; putStr   ")"
    ; putStrLn ""
    }

printVdecl :: [VDecl] -> IO ()
printVdecl []           = return ()
printVdecl (vdel:vdels) = do
    { printIndent 4
    ; printBaseType $ vdelType vdel
    ; putStr " "
    ; putStr $ vdelIdent vdel
    ; printSIndicator $ vdelSIndicator vdel
    ; putStr   ";"
    ; putStrLn ""
    ; printVdecl vdels
    }


printBody :: Body -> IO ()
printBody body = do
    { printVdecl $ bodyVarDeclarations body
    ; putStrLn "begin"
--    ; printStatements
    ; putStrLn "end"
    ; putStrLn ""
    }

printProc :: [Procedure] -> IO ()
printProc []           = return ()
printProc (proc:procs) = do
    { printHeader $ header proc
    ; printBody $ body proc
    ; printProc (procs)
    }

checkMainParam :: [Parameter] -> IO ()
checkMainParam [] = return ()
checkMainParam _  = exitWithError "'main()' procedure should be parameter-less."

checkMainNum :: Int -> IO ()
checkMainNum numMain
    | 0 == numMain = exitWithError "There is no 'main()' procedure."
    | 1 == numMain = return ()
    | otherwise = exitWithError "There is more than one 'main()' procedure"

countMain :: [Procedure] -> [Procedure]
countMain [] = []
countMain (proc:procs)
    | "main" == (headerIdent $ header proc) = proc : countMain procs
    | otherwise = countMain procs


prettyPrint :: GoatProgram -> IO ()
prettyPrint program = do
    { let mainList = countMain $ procedures program
    ; checkMainNum $ length $ mainList
    ; checkMainParam $ parameters $ header $ head mainList
    ; printProc (procedures program)
    ; print $ procedures program
    ; print "finish"
    }
-- prettyPrint program = processProcedure (procedures program)


