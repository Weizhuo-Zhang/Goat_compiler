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
-- Number of white space needed for indentation.
-------------------------------------------------------------------------------
indentationSpaceNumber :: Int
indentationSpaceNumber = 4

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
-- Print semicolon, together with the new line character.
-------------------------------------------------------------------------------
printSemiColon :: IO ()
printSemiColon = putStrLn ";"

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
getVariable variable = (varId variable)
                    ++ (getShapeIndicator $ varShapeIndicator variable)

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
                        ; putStrLn "" -- print new line character
                        }

-------------------------------------------------------------------------------
-- Print variables declaration.
-------------------------------------------------------------------------------
printVariableDeclaration :: [VariableDeclaration] -> IO ()
printVariableDeclaration []                                 = return ()
printVariableDeclaration (variableDeclaration:declarations) = do
    { printIndentation indentationSpaceNumber
    ; printBaseType $ declarationType variableDeclaration
    ; printOneWhiteSpace
    ; putStr $ getVariable $ declarationVariable variableDeclaration
    ; printSemiColon
    ; printVariableDeclaration declarations
    }

-------------------------------------------------------------------------------
-- Print Assignment Statements such as n := 34;
-------------------------------------------------------------------------------
printAssignStatement :: Variable -> Expression -> Int -> IO ()
printAssignStatement variable expression numberOfSpace = do
    { printIndentation numberOfSpace
    ; putStr $ getVariable variable
    ; printOneWhiteSpace
    ; putStr ":="
    ; printOneWhiteSpace
    ; putStr $ getTopExpr expression
    ; printSemiColon
    }

-------------------------------------------------------------------------------
-- Print Read Statements such as read n[3,5];
-------------------------------------------------------------------------------
printReadStatement :: Variable -> Int -> IO ()
printReadStatement variable numberOfSpace = do { printIndentation numberOfSpace
                                               ; putStr "read"
                                               ; printOneWhiteSpace
                                               ; putStr $ getVariable variable
                                               ; printSemiColon
                                               }

-------------------------------------------------------------------------------
-- Print Write Statements such as write 3 + 5;
-------------------------------------------------------------------------------
printWriteStatement :: Expression -> Int -> IO ()
printWriteStatement expression numberOfSpace = do
    { printIndentation numberOfSpace
    ; putStr "write"
    ; printOneWhiteSpace
    ; putStr $ getTopExpr expression
    ; printSemiColon
    }

-------------------------------------------------------------------------------
-- Print Call Statements such as call n(3 + 5);
-------------------------------------------------------------------------------
printCallStatement :: Identifier -> [Expression] -> Int -> IO ()
printCallStatement id expressions numberOfSpace = do
    { printIndentation numberOfSpace
    ; putStr "call "
    ; putStr id
    ; putStr "("
    ; printExprs expressions ""
    ; putStr ")"
    ; printSemiColon
    }

-------------------------------------------------------------------------------
-- Print the common part of If Statements and If-Else Statements.
-------------------------------------------------------------------------------
printIfCommon :: Expression -> [Statement] -> Int -> IO ()
printIfCommon expression statements numberOfSpace = do
    { printIndentation numberOfSpace
    ; putStr "if "
    ; printExprs [expression] ""
    ; putStrLn " then"
    ; printStatements statements (numberOfSpace + indentationSpaceNumber)
    }

-------------------------------------------------------------------------------
-- print the end part of If Statements and If-Else Statements.
-------------------------------------------------------------------------------
printIfEnd :: Int -> IO ()
printIfEnd numberOfSpace = do { printIndentation numberOfSpace
                              ; putStrLn "fi"
                              }

-------------------------------------------------------------------------------
-- Print If Statements.
-------------------------------------------------------------------------------
printIfStatement :: Expression -> [Statement] -> Int -> IO ()
printIfStatement expression statements numberOfSpace = do
    { printIfCommon expression statements numberOfSpace
    ; printIfEnd numberOfSpace
    }

-------------------------------------------------------------------------------
-- Print If-Else Statements
-------------------------------------------------------------------------------
printIfElseStatement :: Expression -> [Statement] -> [Statement] -> Int -> IO ()
printIfElseStatement expression statement1 statement2 numberOfSpace = do
    { printIfCommon expression statement1 numberOfSpace
    ; printIndentation numberOfSpace
    ; putStrLn "else"
    ; printStatements statement2 (numberOfSpace + indentationSpaceNumber)
    ; printIfEnd numberOfSpace
    }

-------------------------------------------------------------------------------
-- Print While Statements.
-------------------------------------------------------------------------------
printWhileStatement :: Expression -> [Statement] -> Int -> IO ()
printWhileStatement expression statements numberOfSpace = do
    { printIndentation numberOfSpace
    ; putStr "while "
    ; printExprs [expression] ""
    ; putStrLn " do"
    ; printStatements statements (numberOfSpace + indentationSpaceNumber)
    ; printIndentation numberOfSpace
    ; putStrLn "od"
    }

-------------------------------------------------------------------------------
-- Print statement.
-------------------------------------------------------------------------------
printStatement :: Statement -> Int -> IO ()
printStatement statement numberOfSpace = do
    case statement of
        Assign variable  expression    -> printAssignStatement variable
                                                               expression
                                                               numberOfSpace
        Read   variable                -> printReadStatement   variable
                                                               numberOfSpace
        Write  expression              -> printWriteStatement  expression
                                                               numberOfSpace
        Call   id   expressions        -> printCallStatement   id
                                                               expressions
                                                               numberOfSpace
        If     expression statements   -> printIfStatement     expression
                                                               statements
                                                               numberOfSpace
        IfElse expression statement1 statement2
                                       -> printIfElseStatement expression
                                                               statement1
                                                               statement2
                                                               numberOfSpace
        While  expression statements   -> printWhileStatement  expression
                                                               statements
                                                               numberOfSpace

-------------------------------------------------------------------------------
-- Print statement list.
-------------------------------------------------------------------------------
printStatements :: [Statement] -> Int -> IO ()
printStatements [] _                                 = return ()
printStatements (statement:statements) numberOfSpace = do
    { printStatement statement numberOfSpace
    ; printStatements statements numberOfSpace
    }

-------------------------------------------------------------------------------
-- Convert constant variable to string.
-------------------------------------------------------------------------------
getConst :: (Show a) => a -> String
getConst a = show a

-------------------------------------------------------------------------------
-- Get string of result using infix operator.
-------------------------------------------------------------------------------
getInfixOpResult :: Expression -> String -> Expression -> String
getInfixOpResult lExpr operator rExpr = (getExpr lExpr)
                                      ++ operator
                                      ++ (getExpr rExpr)

-------------------------------------------------------------------------------
-- Get string of result using prefix operator
-------------------------------------------------------------------------------
getPrefixOpResult :: Expression -> String -> String
getPrefixOpResult expression operator = operator ++ (getExpr expression)

-------------------------------------------------------------------------------
-- Print list of expressionessions.
-------------------------------------------------------------------------------
printExprs :: [Expression] -> String -> IO ()
printExprs [] _                               = return ()
printExprs (expression:expressions) seperator = do
    { putStr seperator
    ; putStr $ getTopExpr expression
    ; printExprs expressions ", "
    }

-------------------------------------------------------------------------------
-- Print the root of expressionesssion which should no surrounded by ().
-------------------------------------------------------------------------------
getTopExpr :: Expression -> String
getTopExpr expression =
    case expression of
        ExprVar     variable   -> getVariable variable
        BoolConst   val        -> getConst val
        IntConst    val        -> getConst val
        FloatConst  val        -> getConst val
        StrConst    val        -> "\"" ++ val ++ "\""
        Add   lExpr rExpr      -> getInfixOpResult  lExpr " + "  rExpr
        Mul   lExpr rExpr      -> getInfixOpResult  lExpr " * "  rExpr
        Sub   lExpr rExpr      -> getInfixOpResult  lExpr " - "  rExpr
        Div   lExpr rExpr      -> getInfixOpResult  lExpr " / "  rExpr
        Or    lExpr rExpr      -> getInfixOpResult  lExpr " || " rExpr
        And   lExpr rExpr      -> getInfixOpResult  lExpr " && " rExpr
        Eq    lExpr rExpr      -> getInfixOpResult  lExpr " = "  rExpr
        NotEq lExpr rExpr      -> getInfixOpResult  lExpr " != " rExpr
        Les   lExpr rExpr      -> getInfixOpResult  lExpr " < "  rExpr
        LesEq lExpr rExpr      -> getInfixOpResult  lExpr " <= " rExpr
        Grt   lExpr rExpr      -> getInfixOpResult  lExpr " > "  rExpr
        GrtEq lExpr rExpr      -> getInfixOpResult  lExpr " >= " rExpr
        UnaryMinus  expression -> getPrefixOpResult expression "-"
        UnaryNot    expression -> getPrefixOpResult expression "!"

-------------------------------------------------------------------------------
-- Wrap the given string with parenthesis.
-------------------------------------------------------------------------------
wrapStringWithParen :: String -> String
wrapStringWithParen value = "(" ++ value ++ ")"

-------------------------------------------------------------------------------
-- Print the non-root of expressionesssion which might surrounded by ().
-------------------------------------------------------------------------------
getExpr :: Expression -> String
getExpr expression =
    let expressionString = getTopExpr expression
    in case expression of
        Add   _ _ -> wrapStringWithParen expressionString
        Mul   _ _ -> wrapStringWithParen expressionString
        Sub   _ _ -> wrapStringWithParen expressionString
        Div   _ _ -> wrapStringWithParen expressionString
        Or    _ _ -> wrapStringWithParen expressionString
        And   _ _ -> wrapStringWithParen expressionString
        Eq    _ _ -> wrapStringWithParen expressionString
        NotEq _ _ -> wrapStringWithParen expressionString
        Les   _ _ -> wrapStringWithParen expressionString
        LesEq _ _ -> wrapStringWithParen expressionString
        Grt   _ _ -> wrapStringWithParen expressionString
        GrtEq _ _ -> wrapStringWithParen expressionString
        otherwise -> expressionString

-------------------------------------------------------------------------------
-- Print Body.
-------------------------------------------------------------------------------
printBody :: Body -> IO ()
printBody body = do { printVariableDeclaration $ bodyVarDeclarations body
                    ; putStrLn "begin"
                    ; printStatements (bodyStatements body) indentationSpaceNumber
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
                            ; putStrLn "" -- print new line character
                            ; printProc (procs)
                            }

-------------------------------------------------------------------------------
-- Check whether the main procedure is parameter-less.
-------------------------------------------------------------------------------
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
-- Get the number of main procedure.
-------------------------------------------------------------------------------
countMain :: [Procedure] -> [Procedure]
countMain [] = []
countMain (proc:procs)
    | "main" == (headerIdent $ header proc) = proc : countMain procs
    | otherwise = countMain procs


-------------------------------------------------------------------------------
-- Main entry of prettyPrint module.
-------------------------------------------------------------------------------
prettyPrint :: GoatProgram -> IO ()
prettyPrint program = do { let mainList = countMain $ procedures program
                         ; checkMainNum $ length $ mainList
                         ; checkMainParam $ parameters $ header $ head mainList
                         ; printProc (procedures program)
                         }
