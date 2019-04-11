module Main where

import GoatAST
import GoatPrettyPrint
import Data.Char
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Q
import System.Environment
import System.Exit

-----------------------------------------------------------------
-- define lexer, reserved words and reserved operator
-----------------------------------------------------------------
type Parser a
    = Parsec String Int a

lexer :: Q.TokenParser Int
lexer
  = Q.makeTokenParser
    (emptyDef
    { Q.commentLine     = "#"
    , Q.nestedComments  = True
    , Q.identStart      = letter
    , Q.identLetter     = alphaNum <|> char '_' <|> char '\''
    , Q.opStart         = oneOf "+-*:"
    , Q.opLetter        = oneOf "+-*:=<>!|&"
    , Q.reservedNames   = myReserved
    , Q.reservedOpNames = myOpnames
    })

whiteSpace = Q.whiteSpace lexer
lexeme     = Q.lexeme lexer
natural    = Q.natural lexer
float      = Q.float lexer
decimal    = Q.decimal lexer
identifier = Q.identifier lexer
colon      = Q.colon lexer
semi       = Q.semi lexer
comma      = Q.comma lexer
dot        = Q.dot lexer
parens     = Q.parens lexer
squares    = Q.squares lexer
reserved   = Q.reserved lexer
reservedOp = Q.reservedOp lexer

myReserved, myOpnames :: [String]

myReserved
  = ["begin", "bool", "call" , "do"  , "else", "end",
     "false", "fi"  , "float", "if"  , "int" , "od",
     "proc" , "read", "ref"  , "then", "true", "val",
     "while", "write"]

myOpnames
  = ["+" , "-" , "*", "<", ">", "<=", ">=", "=", "!=",
     "||", "&&", "!", "/", ":="]

-----------------------------------------------------------------
-- pProg is the topmost parsing function. It looks for a program
-- which contains one or more procedures
-----------------------------------------------------------------

pProg :: Parser GoatProgram
pProg = do
    procedures <- many1 pProcedure
    return (GoatProgram procedures)
    <?> "program procedure"

-----------------------------------------------------------------
-- pProcedure looks for a procedure, which contains "proc"
-- + header + "begin" + body + "end"
-----------------------------------------------------------------

pProcedure :: Parser Procedure
pProcedure = do
    reserved "proc"
    header    <- pProgHeader
    body      <- pProgBody
    return (Procedure header body)
    <?> "procedure"

-----------------------------------------------------------------
-- pProgHeader looks for the program header, which contains a
-- function name followed by serveral parameters and variable
-- declarations
-----------------------------------------------------------------

pProgHeader :: Parser Header
pProgHeader = do
    ident     <- identifier
    char '('
    params    <- sepBy pParameter comma
    char ')'
    newline
    whiteSpace
    return (Header ident params)
    <?> "procedure header"

-----------------------------------------------------------------
-- parameters := (var|ref) (int|float|bool) identifier
-----------------------------------------------------------------
pParameter :: Parser Parameter
pParameter = do
    pidcat    <-  pPIndicator
    ptype     <-  pPtype
    ident     <-  identifier
    return (Parameter pidcat ptype ident)
    <?> "parameters"

pPIndicator :: Parser PIndicator
pPIndicator
  = do { reserved "var"; return VarType }
    <|>
    do { reserved "ref"; return RefType }
    <?> "passing indicator type"

pPtype :: Parser PType
pPtype
  = do { reserved "bool"; return BoolType }
    <|>
    do { reserved "int"; return IntType }
    <|>
    do { reserved "float"; return FloatType }
    <?> "base type indicator"

-----------------------------------------------------------------
-- pProgBody looks for body, which contains one or more statements
-----------------------------------------------------------------

pProgBody :: Parser Body
pProgBody = do
    vdecls <- many pVDecl
    reserved "begin"
    stmts  <- many1 pStmt
    reserved "end"
    return (Body vdecls stmts)
    <?> "procedure body"

-----------------------------------------------------------------
-- cdecl := (int|float|bool) identifier (shape indicator)
-----------------------------------------------------------------

pVDecl :: Parser VDecl
pVDecl = do
    ptype  <- pPtype
    ident  <- identifier
    sidcat <- pSIndicator
    semi
    return (VDecl ptype (Variable ident sidcat))
    <?> "procedure variable declaration"

pSIndicator :: Parser SIndicator
pSIndicator =
    try (do { char '['
            ; n <- pNum
            ; char ']'
            ; return (Array n)
            }
        )
    <|>
    try (do { char '['
            ; m <- pNum
            ; comma
            ; n <- pNum
            ; char ']'
            ; return (Matrix m n)
            })
    <|>  do { return (NoIndicator) }
    <?> "shape indicator"

-----------------------------------------------------------------
-- define statements
-----------------------------------------------------------------

pStmt, pAsg, pRead, pWrite, pCall, pIf, pWhile :: Parser Stmt

pStmt
  = choice [pAsg, pRead, pWrite, pCall, pIf, pWhile]
    <?> "statement"

pRead = do
    reserved "read"
    ident  <- identifier
    sidcat <- pExprSIndicator
    semi
    return (Read (Variable ident sidcat))
    <?> "read statement"

pWrite = do
    reserved "write"
    exp <- (pString <|> pExp)
    semi
    return (Write exp)
    <?> "write statement"

pAsg = do
    ident  <- identifier
    sidcat <- pExprSIndicator
    whiteSpace
    reservedOp ":="
    rvalue <- pExp
    semi
    return (Assign (Variable ident sidcat) rvalue)
    <?> "Assign statement"

pCall = do
    reserved "call"
    ident   <- identifier
    char '('
    expList <- sepBy pExp comma
--    explist <- optional (sepBy pExp comma)
    char ')'
    semi
    return (Call ident expList)
    <?> "Call statement"

pIf =
    try( do
        { reserved "if"
        ; exp   <- pExp
        ; reserved "then"
        ; stmts <- many1 pStmt
        ; reserved "fi"
        ; return (If exp stmts)
        })
    <|> do
        { reserved "if"
        ; exp    <- pExp
        ; reserved "then"
        ; stmts1 <- many1 pStmt
        ; reserved "else"
        ; stmts2 <- many1 pStmt
        ; reserved "fi"
        ; return (IfElse exp stmts1 stmts2)
        }
    <?> "If statement"

-- pIfElse
--   = do
--       reserved "if"
--       exp <- pExp
--       reserved "then"
--       stmts1 <- many1 pStmt
--       reserved "else"
--       stmts2 <- many1 pStmt
--       reserved "fi"
--       return (IfElse exp stmts1 stmts2)

pWhile = do
    reserved "while"
    exp   <- pExp
    reserved "do"
    stmts <- many1 pStmt
    reserved "od"
    return (While exp stmts)
    <?> "While statement"

-----------------------------------------------------------------
-- define expressions
-- Unfinished: 6 relevant operators (<,>,<=,>=,=,!=)
--             and (&&) | or (||)
-----------------------------------------------------------------
pExp, pNum, pIdent, pString, pBool :: Parser Expr
-- pUneg, pUnot, pTerm, pFactor :: Parser Expr
-- pExp
--   = pString
--     <|>
--     pBool
--     -- <|>
--     -- pOp_or
--     -- <|>
--     -- pOp_and
--     <|>
--     (chainl1 pTerm (choice [pOp_add, pOp_min]))
--     <?>
--     "expression"
--
-- pTerm
--   = chainl1 pFactor (choice [pOp_mul, pOp_div])
--     <?>
--     "\"term\""
--
-- pFactor
--   = choice [pUneg, pUnot, parens pExp, pNum, pIdent]
--     <?>
--     "\"factor\""

pExp
 = buildExpressionParser table pFac

pFac :: Parser Expr
pFac = choice [parens pExp, pNum, pIdent, pBool]

table = [[prefix   "-" UnaryMinus]
        ,[binary   "*" Mul, binary   "/"  Div]
        ,[binary   "+" Add, binary   "-"  Sub]
        ,[relation "=" Eq,  relation "!=" NotEq
        , relation "<" Les, relation "<=" LesEq
        , relation ">" Grt, relation ">=" GrtEq]
        ,[prefix   "!" UnaryNot]
        ,[binary   "&&" And]
        ,[binary   "||" Or]]

prefix name func
    = Prefix (do {reservedOp name; return func})

binary name op
    = Infix (do {reservedOp name; return op}) AssocLeft

relation name rel
    = Infix (do {reservedOp name; return rel}) AssocNone

pNum =
     try ( do { n <- many1 digit
           ; char '.' <?> "float"
           ; m <- many1 digit
           ; return (FloatConst (read (n ++ "." ++m) :: Float))
           }
         )
    <|>
           do { n <- natural <?> "integer"
           ; return (IntConst (fromInteger n :: Int))
           }

pExprSIndicator :: Parser SIndicator
pExprSIndicator =
    try (do { char '['
            ; exp   <- pExp
            ; char ']'
            ; return (Array exp)
            }
        )
    <|>
    try (do { char '['
            ; expM   <- pExp
            ; comma
            ; expN   <- pExp
            ; char ']'
            ; return (Matrix expM expN)
            })
    <|>  do { return (NoIndicator) }

pIdent
  = do
      ident <- identifier
      sidcat <- pExprSIndicator
      return (ExprVar (Variable ident sidcat))
      <?>
      "identifier"

pString
  = do
      char '"'
      str <- many (satisfy (/= '"'))
      char '"'
      return (StrConst str)
      <?>
      "string"

pBool
  = do {reserved "true"; return (BoolConst True)}
    <|>
    do {reserved "false"; return (BoolConst False)}
    <?>
    "bool"

-- pUneg
--   = do
--       reservedOp "-"
--       exp <- pFactor
--       return (UnegOp exp)
--
-- pUnot
--   = do
--       reservedOp "!"
--       exp <- pFactor
--       return (UnotOp exp)
--
-- pLvalue :: Parser Lvalue
-- pLvalue
--   = do
--       ident <- identifier
--       return (LId ident)
--       <?>
--       "lvalue"
--
-- pOp_add, pOp_mul, pOp_min, pOp_div, pOp_or, pOp_and, pOp_eq, pOp_neq, pOp_les, pOp_leseq, pOp_grt, pOp_grteq :: Parser (Expr -> Expr -> Expr)

-- pOp_add
--   = do
--     reservedOp "+"
--     return Add
--
-- pOp_mul
--   = do
--     reservedOp "*"
--     return Mul
--
-- pOp_min
--   = do
--     reservedOp "-"
--     return Min
--
-- pOp_div
--   = do
--     reservedOp "/"
--     return Div
--
-- pOp_or
--   = do
--     reservedOp "||"
--     return Or
--
-- pOp_and
--   = do
--     reservedOp "&&"
--     return And
--
-- pOp_eq
--   = do
--     reservedOp "="
--     return Eq
--
-- pOp_neq
--   = do
--     reservedOp "!="
--     return Neq
--
-- pOp_les
--   = do
--     reservedOp "<"
--     return Les
--
-- pOp_leseq
--   = do
--     reservedOp "<="
--     return Leseq
--
-- pOp_grt
--   = do
--     reservedOp ">"
--     return Grt
--
-- pOp_grteq
--   = do
--     reservedOp ">="
--     return Grteq

pMain :: Parser GoatProgram
pMain
  = do
    whiteSpace
    p <- pProg
    eof
    return p

checkArgs :: String -> [String] -> IO ()
checkArgs progName []
  = exitWithError ("Usage: " ++ progName ++ " [-p] fileName\n\n")
checkArgs progName (x:xs)
  = if "-p" == x then
        return ()
    else
        exitWithError ("Sorry, we have not impletement the compiler yet.\n" ++
                "[ERROR] Usage: " ++ progName ++ " [-p] fileName\n\n")
--        ProgramParameters { doPrettyPrint = True
--                          , fileName      = head xs }
-- checkArgs progName _
--  = exitWithError "Sorry, we have not impletement the compiler yet."

main :: IO ()
main
  = do { progName <- getProgName
        ; args <- getArgs
        ; checkArgs progName args
        ; input <- readFile (args !! 1)
        ; let output = runParser pMain 0 "" input
        ; case output of
            Right ast -> prettyPrint ast -- print ast
            Left  err -> do { putStr "Parse error at "
                            ; print err
                            }
        }