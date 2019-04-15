module GoatPrettyPrint where

import GoatAST
import GoatExit
import System.Exit

-------------------------------- Documentation --------------------------------

-- Authors:
--   Shizhe Cai (shizhec) - 798125
--   Weizhuo Zhang (weizhuoz) - 1018329
--   Mingyang Zhang (mingyangz) - 650242
--   An Luo (aluo1) - 657605

-- This file contains the pretty print-related information of the Goat program.

-- The aim of the project is to implement a compiler for a procedural (C-like)
-- language called Goat.

-------------------------------- Documentation --------------------------------

-------------------------------------------------------------------------------
-- Print indention.
-------------------------------------------------------------------------------
printIndentation :: Int -> IO ()
printIndentation 0 = return ()
printIndentation numberOfSpace = do { printOneWhiteSpace
                                    ; printIndentation (numberOfSpace - 1)
                                    }

-------------------------------------------------------------------------------
-- Print one white space.
-------------------------------------------------------------------------------
printOneWhiteSpace :: IO ()
printOneWhiteSpace = putStr " "

-------------------------------------------------------------------------------
-- Print BaseType: bool, float and int.
-------------------------------------------------------------------------------
printBaseType :: BaseType -> IO ()
printBaseType baseType = do
    case baseType of
        BoolType  -> putStr "bool"
        IntType   -> putStr "int"
        FloatType -> putStr "float"

-------------------------------------------------------------------------------
-- Print Passing Indicator type: "val" and "ref".
-------------------------------------------------------------------------------
printParameterIndicator :: ParameterIndicator -> IO ()
printParameterIndicator parameterIndicator = do
    case parameterIndicator of
        VarType   -> putStr "val"
        RefType   -> putStr "ref"

-------------------------------------------------------------------------------
-- Get String of shape Indicator type such as Array and Matrix.
-------------------------------------------------------------------------------
getShapeIndicator :: ShapeIndicator -> String
getShapeIndicator shapeIndicator =
    case shapeIndicator of
        NoIndicator -> ""
        Array  n    -> "[" ++ (getTopExpr n) ++ "]"
        Matrix m n  -> "[" ++ (getTopExpr m) ++ ", " ++ (getTopExpr n) ++ "]"

-------------------------------------------------------------------------------
-- Get String of variable in form id, id[n] or id[m,n].
-------------------------------------------------------------------------------
getVariable :: Variable -> String
getVariable var = (varId var) ++ (getShapeIndicator $ varShapeIndicator var)

-------------------------------------------------------------------------------
-- Print Parameters of procedure
-------------------------------------------------------------------------------
printParameters :: [Parameter] -> String -> IO ()
printParameters [] _                     = return ()
printParameters (param:params) seperator = do
    { putStr seperator
    ; printParameterIndicator $ passingIndicator param
    ; printOneWhiteSpace
    ; printBaseType $ passingType param
    ; printOneWhiteSpace
    ; putStr $ passingIdent param
    ; printParameters (params) ", "
    }

-------------------------------------------------------------------------------
-- Print procedure header.
-------------------------------------------------------------------------------
printHeader :: Header -> IO ()
printHeader header = do { putStr   "proc "
                        ; putStr $ headerIdent $ header
                        ; putStr   " ("
                        ; printParameters (parameters $ header) ""
                        ; putStr   ")"
                        ; putStrLn ""
                        }

-------------------------------------------------------------------------------
-- Print variables declaration.
-------------------------------------------------------------------------------
printVariableDeclaration :: [VariableDeclaration] -> IO ()
printVariableDeclaration []                                 = return ()
printVariableDeclaration (variableDeclaration:declarations) = do
    { printIndentation 4
    ; printBaseType $ declarationType variableDeclaration
    ; printOneWhiteSpace
    ; putStr $ getVariable $ declarationVariable variableDeclaration
    ; putStrLn   ";"
    ; printVariableDeclaration declarations
    }

-------------------------------------------------------------------------------
-- Print Assignment Statements such as n := 34;
-------------------------------------------------------------------------------
printAssignStatement :: Variable -> Expression -> Int -> IO ()
printAssignStatement var expr numberOfSpace = do
    { printIndentation numberOfSpace
    ; putStr $ getVariable var
    ; printOneWhiteSpace
    ; putStr ":="
    ; printOneWhiteSpace
    ; putStr $ getTopExpr expr
    ; putStrLn ";"
    }

-------------------------------------------------------------------------------
-- Print Read Statements such as read n[3,5];
-------------------------------------------------------------------------------
printReadStatement :: Variable -> Int -> IO ()
printReadStatement var numberOfSpace = do { printIndentation numberOfSpace
                                          ; putStr "read"
                                          ; printOneWhiteSpace
                                          ; putStr $ getVariable var
                                          ; putStrLn ";"
                                          }

-------------------------------------------------------------------------------
-- Print Write Statements such as write 3 + 5;
-------------------------------------------------------------------------------
printWriteStmt :: Expression -> Int -> IO ()
printWriteStmt expr numberOfSpace = do { printIndentation numberOfSpace
                                       ; putStr "write"
                                       ; printOneWhiteSpace
                                       ; putStr $ getTopExpr expr
                                       ; putStrLn ";"
                                       }

-------------------------------------------------------------------------------
-- print Call Statements such as call n(3 + 5);
-------------------------------------------------------------------------------
printCallStmt :: Identifier -> [Expression] -> Int -> IO ()
printCallStmt id exprs numberOfSpace = do { printIndentation numberOfSpace
                                   ; putStr "call "
                                   ; putStr id
                                   ; putStr "("
                                   ; printExprs exprs ""
                                   ; putStr ")"
                                   ; putStrLn ";"
                                   }

-------------------------------------------------------------------------------
-- print the common part of If Statements and If-Else Statements
-------------------------------------------------------------------------------
printIfCommon :: Expression -> [Statement] -> Int -> IO ()
printIfCommon expr stmts numberOfSpace = do { printIndentation numberOfSpace
                                     ; putStr "if "
                                     ; printExprs [expr] ""
                                     ; putStrLn " then"
                                     ; printStatements stmts (numberOfSpace + 4)
                                     }

-------------------------------------------------------------------------------
-- print the end part of If Statements and If-Else Statements
-------------------------------------------------------------------------------
printIfEnd :: Int -> IO ()
printIfEnd numberOfSpace = do { printIndentation numberOfSpace
                       ; putStrLn "fi"
                       }

-------------------------------------------------------------------------------
-- print If Statements
-------------------------------------------------------------------------------
printIfStatement :: Expression -> [Statement] -> Int -> IO ()
printIfStatement expr stmts numberOfSpace = do { printIfCommon expr stmts numberOfSpace
                                        ; printIfEnd numberOfSpace
                                        }

-------------------------------------------------------------------------------
-- print If-Else Statements
-------------------------------------------------------------------------------
printIfElseStatement :: Expression -> [Statement] -> [Statement] -> Int -> IO ()
printIfElseStatement expr stmts1 stmts2 numberOfSpace = do
    { printIfCommon expr stmts1 numberOfSpace
    ; printIndentation numberOfSpace
    ; putStrLn "else"
    ; printStatements stmts2 (numberOfSpace + 4)
    ; printIfEnd numberOfSpace
    }

-------------------------------------------------------------------------------
-- print While Statements
-------------------------------------------------------------------------------
printWhileStatement :: Expression -> [Statement] -> Int -> IO ()
printWhileStatement expr stmts numberOfSpace = do { printIndentation numberOfSpace
                                           ; putStr "while "
                                           ; printExprs [expr] ""
                                           ; putStrLn " do"
                                           ; printStatements stmts (numberOfSpace + 4)
                                           ; printIndentation numberOfSpace
                                           ; putStrLn "od"
                                           }

-------------------------------------------------------------------------------
-- print Statement
-------------------------------------------------------------------------------
printStatement :: Statement -> Int -> IO ()
printStatement stmt numberOfSpace = do
    case stmt of
        Assign var  expr          -> printAssignStatement var  expr   numberOfSpace
        Read   var                -> printReadStatement   var  numberOfSpace
        Write  expr               -> printWriteStmt  expr numberOfSpace
        Call   id   exprs         -> printCallStmt   id   exprs  numberOfSpace
        If     expr stmts         -> printIfStatement     expr stmts  numberOfSpace
        IfElse expr stmts1 stmts2 -> printIfElseStatement expr stmts1 stmts2 numberOfSpace
        While  expr stmts         -> printWhileStatement  expr stmts  numberOfSpace

-------------------------------------------------------------------------------
-- print list of Statement
-------------------------------------------------------------------------------
printStatements :: [Statement] -> Int -> IO ()
printStatements [] _                = return ()
printStatements (stmt:stmts) numberOfSpace = do { printStatement stmt numberOfSpace
                                         ; printStatements stmts numberOfSpace
                                         }

-------------------------------------------------------------------------------
-- converte const variable to String
-------------------------------------------------------------------------------
getConst :: (Show a) => a -> String
getConst a = show a

-------------------------------------------------------------------------------
-- get string of result using infix operator
-------------------------------------------------------------------------------
getInfixOpResult :: Expression -> String -> Expression -> String
getInfixOpResult lExpr op rExpr = (getExpr lExpr) ++ op ++ (getExpr rExpr)

-------------------------------------------------------------------------------
-- get string of result using prefix operator
-------------------------------------------------------------------------------
getPrefixOpResult :: Expression -> String -> String
getPrefixOpResult expr op = op ++ (getExpr expr)

-------------------------------------------------------------------------------
-- print list of expressions
-------------------------------------------------------------------------------
printExprs :: [Expression] -> String -> IO ()
printExprs [] _                   = return ()
printExprs (expr:exprs) seperator = do { putStr seperator
                                       ; putStr $ getTopExpr expr
                                       ; printExprs exprs ", "
                                       }

-------------------------------------------------------------------------------
-- print the root of expresssion which should no surrounded by ()
-------------------------------------------------------------------------------
getTopExpr :: Expression -> String
getTopExpr expr =
    case expr of
        ExprVar     var   -> getVariable var
        BoolConst   val   -> getConst val
        IntConst    val   -> getConst val
        FloatConst  val   -> getConst val
        StrConst    val   -> "\"" ++ val ++ "\""
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

-------------------------------------------------------------------------------
-- print the non-root of expresssion which might surrounded by ()
-------------------------------------------------------------------------------
getExpr :: Expression -> String
getExpr expr =
    case expr of
        ExprVar     var   -> getVariable var
        BoolConst   val   -> getConst val
        IntConst    val   -> getConst val
        FloatConst  val   -> getConst val
        StrConst    val   -> "\"" ++ val ++ "\""
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

-------------------------------------------------------------------------------
-- print Body
-------------------------------------------------------------------------------
printBody :: Body -> IO ()
printBody body = do { printVariableDeclaration $ bodyVarDeclarations body
                    ; putStrLn "begin"
                    ; printStatements (bodyStatements body) 4
                    ; putStrLn "end"
                    }

-------------------------------------------------------------------------------
-- print Procedure
-------------------------------------------------------------------------------
printProc :: [Procedure] -> IO ()
printProc []           = return ()
printProc (proc:[])    = do { printHeader $ header proc
                            ; printBody $ body proc
                            }
printProc (proc:procs) = do { printHeader $ header proc
                            ; printBody $ body proc
                            ; putStrLn ""
                            ; printProc (procs)
                            }

-------------------------------------------------------------------------------
-- check whether the main procedure is parameter-less
-------------------------------------------------------------------------------
checkMainParam :: [Parameter] -> IO Task
checkMainParam [] = return Unit
checkMainParam _  = do
  exitWithError "'main()' procedure should be parameter-less." MainWithParam

-------------------------------------------------------------------------------
-- check the number of main procedure
-------------------------------------------------------------------------------
checkMainNum :: Int -> IO Task
checkMainNum numMain
    | 0 == numMain = do
        exitWithError "There is no 'main()' procedure." MissingMain
    | 1 == numMain = return Unit
    | otherwise = do
        exitWithError "There is more than one 'main()' procedure" MultipleMain

-------------------------------------------------------------------------------
-- get the number of main procedure
-------------------------------------------------------------------------------
countMain :: [Procedure] -> [Procedure]
countMain [] = []
countMain (proc:procs)
    | "main" == (headerIdent $ header proc) = proc : countMain procs
    | otherwise = countMain procs


-------------------------------------------------------------------------------
-- main entry of prettyPrint module
-------------------------------------------------------------------------------
prettyPrint :: GoatProgram -> IO ()
prettyPrint program = do { let mainList = countMain $ procedures program
                         ; checkMainNum $ length $ mainList
                         ; checkMainParam $ parameters $ header $ head mainList
                         ; printProc (procedures program)
                         }
