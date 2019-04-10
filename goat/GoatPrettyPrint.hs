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

printPassIndicator :: PIndicator -> IO ()
printPassIndicator pIndicator = do
    case pIndicator of
        VarType   -> putStr "var"
        RefType   -> putStr "ref"

--printSIndicator :: SIndicator -> IO ()
--printSIndicator sIndicator = do
--    case sIndicator of
--        NoIndicator -> return ()
--        Array  n    -> putStr $ "[" ++ (printExpr n) ++ "]"
--        Matrix m n  -> putStr $ "[" ++ (printExpr m) ++ ", " ++ (printExpr n) ++ "]"

--printVariable :: Variable -> IO ()
--printVariable var = do
--    { putStr $ varId var
--    ; printSIndicator $ varSIndicator var
--    }

getSIndicator :: SIndicator -> String
getSIndicator sIndicator =
    case sIndicator of
        NoIndicator -> ""
        Array  n    -> "[" ++ (getTopExpr n) ++ "]"
        Matrix m n  -> "[" ++ (getTopExpr m) ++ ", " ++ (getTopExpr n) ++ "]"

getVariable :: Variable -> String
getVariable var = (varId var) ++ (getSIndicator $ varSIndicator var)

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
printVdecl (vdecl:vdels) = do
    { printIndent 4
    ; printBaseType $ vdeclType vdecl
    ; putStr " "
    ; putStr $ getVariable $ vdeclVar vdecl
--    ; printVariable $ vdeclVar vdecl
    ; putStr   ";"
    ; putStrLn ""
    ; printVdecl vdels
    }

printAssignStmt :: Variable -> Expr -> Int -> IO ()
printAssignStmt var expr indent = do
    { printIndent indent
    ; putStr $ getVariable var
    ; putStrLn " := "
    ; putStr $ getTopExpr expr
    ; putStrLn ""
    }

printStatements :: [Stmt] -> Int -> IO ()
printStatements [] _                = return ()
printStatements (stmt:stmts) indent = do
    case stmt of
        Assign var expr -> printAssignStmt var expr indent
--        Read Lvalue
--        Write Expr
--        Call Lvalue ()
--        If Expr [Stmt]
--        IfElse Expr [Stmt] [Stmt]
--        While Expr [Stmt]

getTopExpr :: Expr -> String
getTopExpr expr =
    case expr of
        ExprVar     var   -> getVariable var
        BoolConst   val   -> show val
        IntConst    val   -> show val
        FloatConst  val   -> show val
        StrConst    val   -> show val
        Add   lExpr rExpr -> (getExpr lExpr) ++ " + "  ++ (getExpr lExpr)


getExpr :: Expr -> String
getExpr expr =
    case expr of
--        Add   lExpr rExpr ->
--            putStr ("(" ++ (printExpr lExpr) ++ " + "  ++ (printExpr lExpr) ++ ")")
--        Mul   lExpr rExpr ->
--            putStr ("(" ++ (printExpr lExpr) ++ " * " ++ (printExpr lExpr) ++ ")")
--        Sub   lExpr rExpr ->
--            putStr ("(" ++ (printExpr lExpr) ++ " - "  ++ (printExpr lExpr) ++ ")")
--        Div   lExpr rExpr ->
--            putStr ("(" ++ (printExpr lExpr) ++ " / "  ++ (printExpr lExpr) ++ ")")
--        Or    lExpr rExpr ->
--            putStr ("(" ++ (printExpr lExpr) ++ " || " ++ (printExpr lExpr) ++ ")")
--        And   lExpr rExpr ->
--            putStr ("(" ++ (printExpr lExpr) ++ " && " ++ (printExpr lExpr) ++ ")")
--        Eq    lExpr rExpr ->
--            putStr ("(" ++ (printExpr lExpr) ++ " = "  ++ (printExpr lExpr) ++ ")")
--        NotEq lExpr rExpr ->
--            putStr ("(" ++ (printExpr lExpr) ++ " != " ++ (printExpr lExpr) ++ ")")
--        Les   lExpr rExpr ->
--            putStr ("(" ++ (printExpr lExpr) ++ " < "  ++ (printExpr lExpr) ++ ")")
--        LesEq lExpr rExpr ->
--            putStr ("(" ++ (printExpr lExpr) ++ " <= " ++ (printExpr lExpr) ++ ")")
--        Grt   lExpr rExpr ->
--            putStr ("(" ++ (printExpr lExpr) ++ " > "  ++ (printExpr lExpr) ++ ")")
--        GrtEq lExpr rExpr ->
--            putStr ("(" ++ (printExpr lExpr) ++ " >= " ++ (printExpr lExpr) ++ ")")
--        UnaryMinus  expr  -> putStr ("-" ++ (printExpr expr))
--        UnaryNot    expr  -> putStr ("!" ++ (printExpr expr))


--printExpr :: Expr -> IO ()
--printExpr expr = do
--    case expr of
--        ExprVar     var   -> printVariable var
--        BoolConst   val   -> show val
--        IntConst    val   -> show val
--        FloatConst  val   -> show val
--        StrConst    val   -> show val
--        Add   lExpr rExpr ->
--            putStr ("(" ++ (printExpr lExpr) ++ " + "  ++ (printExpr lExpr) ++ ")")
--        Mul   lExpr rExpr ->
--            putStr ("(" ++ (printExpr lExpr) ++ " * " ++ (printExpr lExpr) ++ ")")
--        Sub   lExpr rExpr ->
--            putStr ("(" ++ (printExpr lExpr) ++ " - "  ++ (printExpr lExpr) ++ ")")
--        Div   lExpr rExpr ->
--            putStr ("(" ++ (printExpr lExpr) ++ " / "  ++ (printExpr lExpr) ++ ")")
--        Or    lExpr rExpr ->
--            putStr ("(" ++ (printExpr lExpr) ++ " || " ++ (printExpr lExpr) ++ ")")
--        And   lExpr rExpr ->
--            putStr ("(" ++ (printExpr lExpr) ++ " && " ++ (printExpr lExpr) ++ ")")
--        Eq    lExpr rExpr ->
--            putStr ("(" ++ (printExpr lExpr) ++ " = "  ++ (printExpr lExpr) ++ ")")
--        NotEq lExpr rExpr ->
--            putStr ("(" ++ (printExpr lExpr) ++ " != " ++ (printExpr lExpr) ++ ")")
--        Les   lExpr rExpr ->
--            putStr ("(" ++ (printExpr lExpr) ++ " < "  ++ (printExpr lExpr) ++ ")")
--        LesEq lExpr rExpr ->
--            putStr ("(" ++ (printExpr lExpr) ++ " <= " ++ (printExpr lExpr) ++ ")")
--        Grt   lExpr rExpr ->
--            putStr ("(" ++ (printExpr lExpr) ++ " > "  ++ (printExpr lExpr) ++ ")")
--        GrtEq lExpr rExpr ->
--            putStr ("(" ++ (printExpr lExpr) ++ " >= " ++ (printExpr lExpr) ++ ")")
--        UnaryMinus  expr  -> putStr ("-" ++ (printExpr expr))
--        UnaryNot    expr  -> putStr ("!" ++ (printExpr expr))



printBody :: Body -> IO ()
printBody body = do
    { printVdecl $ bodyVarDeclarations body
    ; putStrLn "begin"
    ; printStatements (bodyStatements body) 4
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


