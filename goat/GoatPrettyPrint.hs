module GoatPrettyPrint where

import GoatAST
import GoatExit
import System.Exit

-- TODO should trace line number
-- exitWithSuccess :: String -> IO ()
-- exitWithSuccess message = do
--   putStrLn message
--     exitWith ExitSuccess

-----------------------------------------------------------------
-- print error message to stderr and exit
-----------------------------------------------------------------
-- exitWithError :: String -> Int -> IO ()
-- exitWithError message  = do
--     putStrLn ("[ERROR] " ++ message)
--     exitWith (ExitFailure )

-----------------------------------------------------------------
-- print indention
-----------------------------------------------------------------
printIndent :: Int -> IO ()
printIndent 0 = return ()
printIndent indent = do
    { putStr " "
    ; printIndent (indent - 1)
    }

-----------------------------------------------------------------
-- print BaseType such as bool, float and int
-----------------------------------------------------------------
printBaseType :: PType -> IO ()
printBaseType baseType = do
    case baseType of
        BoolType  -> putStr "bool"
        IntType   -> putStr "int"
        FloatType -> putStr "float"

-----------------------------------------------------------------
-- print Passing Indicator type such as "var" and "ref"
-----------------------------------------------------------------
printPassIndicator :: PIndicator -> IO ()
printPassIndicator pIndicator = do
    case pIndicator of
        VarType   -> putStr "var"
        RefType   -> putStr "ref"

-----------------------------------------------------------------
-- get String of shape Indicator type such as Array and Matrix
-----------------------------------------------------------------
getSIndicator :: SIndicator -> String
getSIndicator sIndicator =
    case sIndicator of
        NoIndicator -> ""
        Array  n    -> "[" ++ (getTopExpr n) ++ "]"
        Matrix m n  -> "[" ++ (getTopExpr m) ++ ", " ++ (getTopExpr n) ++ "]"

-----------------------------------------------------------------
-- get String of variable in form id, id[n] or id[m,n]
-----------------------------------------------------------------
getVariable :: Variable -> String
getVariable var = (varId var) ++ (getSIndicator $ varSIndicator var)

-----------------------------------------------------------------
-- print Parameters of procedure
-----------------------------------------------------------------
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

-----------------------------------------------------------------
-- print Header
-----------------------------------------------------------------
printHeader :: Header -> IO ()
printHeader header = do
    { putStr   "proc "
    ; putStr $ headerIdent $ header
    ; putStr   " ("
    ; printParameters (parameters $ header) ""
    ; putStr   ")"
    ; putStrLn ""
    }

-----------------------------------------------------------------
-- print variables declaration
-----------------------------------------------------------------
printVdecl :: [VDecl] -> IO ()
printVdecl []           = return ()
printVdecl (vdecl:vdels) = do
    { printIndent 4
    ; printBaseType $ vdeclType vdecl
    ; putStr " "
    ; putStr $ getVariable $ vdeclVar vdecl
    ; putStr   ";"
    ; putStrLn ""
    ; printVdecl vdels
    }

-----------------------------------------------------------------
-- print Assignment Statements such as n := 34;
-----------------------------------------------------------------
printAssignStmt :: Variable -> Expr -> Int -> IO ()
printAssignStmt var expr indent = do
    { printIndent indent
    ; putStr $ getVariable var
    ; putStr " := "
    ; putStr $ getTopExpr expr
    ; putStrLn ";"
    }

-----------------------------------------------------------------
-- print Read Statements such as read n[3,5];
-----------------------------------------------------------------
printReadStmt :: Variable -> Int -> IO ()
printReadStmt var indent = do
    { printIndent indent
    ; putStr "read "
    ; putStr $ getVariable var
    ; putStrLn ";"
    }

-----------------------------------------------------------------
-- print Write Statements such as write 3 + 5;
-----------------------------------------------------------------
printWriteStmt :: Expr -> Int -> IO ()
printWriteStmt expr indent = do
    { printIndent indent
    ; putStr "write "
    ; putStr $ getTopExpr expr
    ; putStrLn ";"
    }

-----------------------------------------------------------------
-- print Call Statements such as call n(3 + 5);
-----------------------------------------------------------------
printCallStmt :: Ident -> [Expr] -> Int -> IO ()
printCallStmt id exprs indent = do
    { printIndent indent
    ; putStr "call "
    ; putStr id
    ; putStr "("
    ; printExprs exprs ""
    ; putStr ")"
    ; putStrLn ";"
    }

-----------------------------------------------------------------
-- print the common part of If Statements and If-Else Statements
-----------------------------------------------------------------
printIfCommon :: Expr -> [Stmt] -> Int -> IO ()
printIfCommon expr stmts indent = do
    { printIndent indent
    ; putStr "if "
    ; printExprs [expr] ""
    ; putStrLn " then"
    ; printStatements stmts (indent + 4)
    }

-----------------------------------------------------------------
-- print the end part of If Statements and If-Else Statements
-----------------------------------------------------------------
printIfEnd :: Int -> IO ()
printIfEnd indent = do
    { printIndent indent
    ; putStrLn "fi"
    }

-----------------------------------------------------------------
-- print If Statements
-----------------------------------------------------------------
printIfStmt :: Expr -> [Stmt] -> Int -> IO ()
printIfStmt expr stmts indent = do
    { printIfCommon expr stmts indent
    ; printIfEnd indent
    }

-----------------------------------------------------------------
-- print If-Else Statements
-----------------------------------------------------------------
printIfElseStmt :: Expr -> [Stmt] -> [Stmt] -> Int -> IO ()
printIfElseStmt expr stmts1 stmts2 indent = do
    { printIfCommon expr stmts1 indent
    ; printIndent indent
    ; putStrLn "else"
    ; printStatements stmts2 (indent + 4)
    ; printIfEnd indent
    }

-----------------------------------------------------------------
-- print While Statements
-----------------------------------------------------------------
printWhileStmt :: Expr -> [Stmt] -> Int -> IO ()
printWhileStmt expr stmts indent = do
    { printIndent indent
    ; putStr "while "
    ; printExprs [expr] ""
    ; putStrLn " do"
    ; printStatements stmts (indent + 4)
    ; printIndent indent
    ; putStrLn "od"
    }

-----------------------------------------------------------------
-- print Statement
-----------------------------------------------------------------
printStatement :: Stmt -> Int -> IO ()
printStatement stmt indent = do
    case stmt of
        Assign var  expr          -> printAssignStmt var  expr   indent
        Read   var                -> printReadStmt   var  indent
        Write  expr               -> printWriteStmt  expr indent
        Call   id   exprs         -> printCallStmt   id   exprs  indent
        If     expr stmts         -> printIfStmt     expr stmts  indent
        IfElse expr stmts1 stmts2 -> printIfElseStmt expr stmts1 stmts2 indent
        While  expr stmts         -> printWhileStmt  expr stmts  indent

-----------------------------------------------------------------
-- print list of Statement
-----------------------------------------------------------------
printStatements :: [Stmt] -> Int -> IO ()
printStatements [] _                = return ()
printStatements (stmt:stmts) indent = do
    { printStatement stmt indent
    ; printStatements stmts indent
    }

-----------------------------------------------------------------
-- converte const variable to String
-----------------------------------------------------------------
getConst :: (Show a) => a -> String
getConst a = show a

-----------------------------------------------------------------
-- get string of result using infix operator
-----------------------------------------------------------------
getInfixOpResult :: Expr -> String -> Expr -> String
getInfixOpResult lExpr op rExpr = (getExpr lExpr) ++ op ++ (getExpr rExpr)

-----------------------------------------------------------------
-- get string of result using prefix operator
-----------------------------------------------------------------
getPrefixOpResult :: Expr -> String -> String
getPrefixOpResult expr op = op ++ (getExpr expr)

-----------------------------------------------------------------
-- print list of expressions
-----------------------------------------------------------------
printExprs :: [Expr] -> String -> IO ()
printExprs [] _                   = return ()
printExprs (expr:exprs) seperator = do
    { putStr seperator
    ; putStr $ getTopExpr expr
    ; printExprs exprs ", "
    }

-----------------------------------------------------------------
-- print the root of expresssion which should no surrounded by ()
-----------------------------------------------------------------
getTopExpr :: Expr -> String
getTopExpr expr =
    case expr of
        ExprVar     var   -> getVariable var
        BoolConst   val   -> getConst val
        IntConst    val   -> getConst val
        FloatConst  val   -> getConst val
        StrConst    val   -> getConst val
        Add   lExpr rExpr -> getInfixOpResult  lExpr " + "  rExpr
        Mul   lExpr rExpr -> getInfixOpResult  lExpr " * "  rExpr
        Sub   lExpr rExpr -> getInfixOpResult  lExpr " - "  rExpr
        Div   lExpr rExpr -> getInfixOpResult  lExpr " / "  rExpr
        Or    lExpr rExpr -> getInfixOpResult  lExpr " || " rExpr
        And   lExpr rExpr -> getInfixOpResult  lExpr " && " rExpr
        Eq    lExpr rExpr -> getInfixOpResult  lExpr " = "  rExpr
        NotEq lExpr rExpr -> getInfixOpResult  lExpr " != " rExpr
        Les   lExpr rExpr -> getInfixOpResult  lExpr " < "  rExpr
        LesEq lExpr rExpr -> getInfixOpResult  lExpr " <= " rExpr
        Grt   lExpr rExpr -> getInfixOpResult  lExpr " > "  rExpr
        GrtEq lExpr rExpr -> getInfixOpResult  lExpr " >= " rExpr
        UnaryMinus  expr  -> getPrefixOpResult expr "-"
        UnaryNot    expr  -> getPrefixOpResult expr "!"

-----------------------------------------------------------------
-- print the non-root of expresssion which might surrounded by ()
-----------------------------------------------------------------
getExpr :: Expr -> String
getExpr expr =
    case expr of
        ExprVar     var   -> getVariable var
        BoolConst   val   -> getConst val
        IntConst    val   -> getConst val
        FloatConst  val   -> getConst val
        StrConst    val   -> getConst val
        Add   lExpr rExpr -> "(" ++ getInfixOpResult lExpr " + "  rExpr ++ ")"
        Mul   lExpr rExpr -> "(" ++ getInfixOpResult lExpr " * "  rExpr ++ ")"
        Sub   lExpr rExpr -> "(" ++ getInfixOpResult lExpr " - "  rExpr ++ ")"
        Div   lExpr rExpr -> "(" ++ getInfixOpResult lExpr " / "  rExpr ++ ")"
        Or    lExpr rExpr -> "(" ++ getInfixOpResult lExpr " || " rExpr ++ ")"
        And   lExpr rExpr -> "(" ++ getInfixOpResult lExpr " && " rExpr ++ ")"
        Eq    lExpr rExpr -> "(" ++ getInfixOpResult lExpr " = "  rExpr ++ ")"
        NotEq lExpr rExpr -> "(" ++ getInfixOpResult lExpr " != " rExpr ++ ")"
        Les   lExpr rExpr -> "(" ++ getInfixOpResult lExpr " < "  rExpr ++ ")"
        LesEq lExpr rExpr -> "(" ++ getInfixOpResult lExpr " <= " rExpr ++ ")"
        Grt   lExpr rExpr -> "(" ++ getInfixOpResult lExpr " > "  rExpr ++ ")"
        GrtEq lExpr rExpr -> "(" ++ getInfixOpResult lExpr " >= " rExpr ++ ")"
        UnaryMinus  expr  -> getPrefixOpResult expr "-"
        UnaryNot    expr  -> getPrefixOpResult expr "!"

-----------------------------------------------------------------
-- print Body
-----------------------------------------------------------------
printBody :: Body -> IO ()
printBody body = do
    { printVdecl $ bodyVarDeclarations body
    ; putStrLn "begin"
    ; printStatements (bodyStatements body) 4
    ; putStrLn "end"
    ; putStrLn ""
    }

-----------------------------------------------------------------
-- print Procedure
-----------------------------------------------------------------
printProc :: [Procedure] -> IO ()
printProc []           = return ()
printProc (proc:procs) = do
    { printHeader $ header proc
    ; printBody $ body proc
    ; printProc (procs)
    }

-----------------------------------------------------------------
-- check whether the main procedure is parameter-less
-----------------------------------------------------------------
checkMainParam :: [Parameter] -> IO Task
checkMainParam [] = return Unit
checkMainParam _  = exitWithError "'main()' procedure should be parameter-less." MainWithParam

-----------------------------------------------------------------
-- check the number of main procedure
-----------------------------------------------------------------
checkMainNum :: Int -> IO Task
checkMainNum numMain
    | 0 == numMain = exitWithError "There is no 'main()' procedure." MissingMain
    | 1 == numMain = return Unit
    | otherwise = exitWithError "There is more than one 'main()' procedure" MultipleMain

-----------------------------------------------------------------
-- get the number of main procedure
-----------------------------------------------------------------
countMain :: [Procedure] -> [Procedure]
countMain [] = []
countMain (proc:procs)
    | "main" == (headerIdent $ header proc) = proc : countMain procs
    | otherwise = countMain procs


-----------------------------------------------------------------
-- main entry of prettyPrint module
-----------------------------------------------------------------
prettyPrint :: GoatProgram -> IO ()
prettyPrint program = do
    { let mainList = countMain $ procedures program
    ; checkMainNum $ length $ mainList
    ; checkMainParam $ parameters $ header $ head mainList
    ; printProc (procedures program)
    }
