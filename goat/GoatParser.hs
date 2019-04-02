module Main where

import GoatAST
import Data.Char
import Text.Parsec
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
    { Q.commentLine    = "#"
    , Q.nestedComments = True
    , Q.identStart     = letter
    , Q.identLetter    = alphaNum <|> char '_'
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
  = ["begin", "bool", "do", "else", "end",
  "false", "fi", "float", "if", "int", "od",
     "proc", "read", "ref", "then", "true", "val",
     "while", "write", "call"]

myOpnames
  = ["+", "-", "*", "<", ">", "<=", ">=", "=", "!=",
    "||", "&&", "!", "/", ":="]

-----------------------------------------------------------------
-- pProg is the topmost parsing function. It looks for a program
-- which contains one or more procedures
-----------------------------------------------------------------

pProg :: Parser GoatProgram
pProg
  = do
      procedures <- many1 pProcedure
      return (Program procedures)

-----------------------------------------------------------------
-- pProcedure looks for a procedure, which contains "proc"
-- + header + "begin" + body + "end"
-----------------------------------------------------------------

pProcedure :: Parser Procedure
pProcedure
  = do
    reserved "proc"
    header    <- pProgHeader
    reserved "begin"
    body      <- pProgBody
    reserved "end"
    return (Procedure header body)

-----------------------------------------------------------------
-- pProgHeader looks for the program header, which contains a
-- function name followed by serveral parameters and variable
-- declarations
-----------------------------------------------------------------

pProgHeader :: Parser Header
pProgHeader
  = do
    ident     <- identifier
    char '('
    params    <- sepBy pParameter comma
    char ')'
    newline
    whiteSpace
    vdecls    <- many pVDecl
    return (Header ident params vdecls)

-----------------------------------------------------------------
-- parameters := (var|ref) (int|float|bool) identifier
-----------------------------------------------------------------
pParameter :: Parser Parameter
pParameter
  = do
    pidcat    <-  pPindicator
    ptype     <-  pPtype
    ident     <-  identifier
    return (Parameter pidcat ptype ident)

pPindicator :: Parser Pindicator
pPindicator
  = do { reserved "var"; return VarType }
    <|>
    do { reserved "ref"; return RefType }

pPtype :: Parser PType
pPtype
  = do { reserved "bool"; return BoolType }
    <|>
    do { reserved "int"; return IntType }
    <|>
    do { reserved "float"; return FloatType }

-----------------------------------------------------------------
-- pProgBody looks for body, which contains one or more statements
-----------------------------------------------------------------

pProgBody :: Parser Body
pProgBody
  = do
    stmts  <- many1 pStmt
    return (Body stmts)

-----------------------------------------------------------------
-- cdecl := (int|float|bool) identifier (shape indicator)
-----------------------------------------------------------------

pVDecl :: Parser VDecl
pVDecl
  = do
    ptype   <-   pPtype
    ident   <-   identifier
    sidcat  <- optional pSindicator
    semi
    return (VDecl ptype ident sidcat)

pSindicator :: Parser Sindicator
pSindicator
  = do { char '[';
          n <- pInt;
          comma;
          m <- pInt;
         char ']' ; return (Matrix (n,m))}
    <|>
    do {
        char '[';
        n <- pInt;
        char ']' ; return (Array n)
      }

-----------------------------------------------------------------
-- define statements
-----------------------------------------------------------------

pStmt, pAsg, pRead, pWrite, pCall, pIf, pIfElse, pWhile :: Parser Stmt

pStmt
  = choice [pAsg, pRead, pWrite, pCall, pIf, pIfElse, pWhile]

pRead
  = do
      reserved "read"
      lvalue <- pLvalue
      semi
      return (Read lvalue)

pWrite
  = do
      reserved "write"
      exp <- (pString <|> pExp)
      semi
      return (Write exp)

pAsg
  = do
      lvalue <- pLvalue
      reservedOp ":="
      rvalue <- pExp
      semi
      return (Assign lvalue rvalue)

pCall
  = do
      reserved "call"
      lvalue <- pLvalue
      explist <- optional (sepBy pExp comma)
      semi
      return (Call lvalue explist)

pIf
  = do
      reserved "if"
      exp <- pExp
      reserved "then"
      stmts <- many1 pStmt
      reserved "fi"
      semi
      return (If exp stmts)

pIfElse
  = do
      reserved "if"
      exp <- pExp
      reserved "then"
      stmts1 <- many1 pStmt
      reserved "else"
      stmts2 <- many1 pStmt
      reserved "fi"
      semi
      return (IfElse exp stmts1 stmts2)

pWhile
  = do
    reserved "while"
    exp <- pExp
    reserved "do"
    stmts <- many1 pStmt
    reserved "od"
    semi
    return (While exp stmts)

-----------------------------------------------------------------
-- define expressions
-- Unfinished: 6 relevant operators (<,>,<=,>=,=,!=)
--             and (&&) | or (||)
-----------------------------------------------------------------
pExp, pTerm, pFactor, pInt, pFloat, pIdent, pString, pUneg, pUnot, pBool :: Parser Expr

pExp
  = pString
    <|>
    pBool
    -- <|>
    -- pOp_or
    -- <|>
    -- pOp_and
    <|>
    (chainl1 pTerm (choice [pOp_add, pOp_min]))
    <?>
    "expression"

pTerm
  = chainl1 pFactor (choice [pOp_mul, pOp_div])
    <?>
    "\"term\""

pFactor
  = choice [pUneg, pUnot, parens pExp, pInt, pFloat, pIdent]
    <?>
    "\"factor\""

pInt
  = do
    n <- natural <?> "integer"
    return (IntConst (fromInteger n :: Int))

-- float needs debug
pFloat
  = do
    n <- decimal <?> "float"
    return (FloatConst (fromInteger n :: Float))

pIdent
  = do
      ident <- identifier
      return (Id ident)
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

pUneg
  = do
      reservedOp "-"
      exp <- pFactor
      return (UnegOp exp)

pUnot
  = do
      reservedOp "!"
      exp <- pFactor
      return (UnotOp exp)


pLvalue :: Parser Lvalue
pLvalue
  = do
      ident <- identifier
      return (LId ident)
      <?>
      "lvalue"

pOp_add, pOp_mul, pOp_min, pOp_div, pOp_or, pOp_and, pOp_eq, pOp_neq, pOp_les, pOp_leseq, pOp_grt, pOp_grteq :: Parser (Expr -> Expr -> Expr)

pOp_add
  = do
    reservedOp "+"
    return Add

pOp_mul
  = do
    reservedOp "*"
    return Mul

pOp_min
  = do
    reservedOp "-"
    return Min

pOp_div
  = do
    reservedOp "/"
    return Div

pOp_or
  = do
    reservedOp "||"
    return Or

pOp_and
  = do
    reservedOp "&&"
    return And

pOp_eq
  = do
    reservedOp "="
    return Eq

pOp_neq
  = do
    reservedOp "!="
    return Neq

pOp_les
  = do
    reservedOp "<"
    return Les

pOp_leseq
  = do
    reservedOp "<="
    return Leseq

pOp_grt
  = do
    reservedOp ">"
    return Grt

pOp_grteq
  = do
    reservedOp ">="
    return Grteq

pMain :: Parser GoatProgram
pMain
  = do
    whiteSpace
    p <- pProg
    eof
    return p

main :: IO ()
main
  = do { progname <- getProgName
        ; args <- getArgs
        ; checkArgs progname args
        ; input <- readFile (head args)
        ; let output = runParser pMain 0 "" input
        ; case output of
            Right ast -> print ast
            Left  err -> do { putStr "Parse error at "
                            ; print err
                            }
        }

checkArgs :: String -> [String] -> IO ()
checkArgs _ [filename]
  = return ()
checkArgs progname _
  = do { putStrLn ("Usage: " ++ progname ++ " filename\n\n")
      ; exitWith (ExitFailure 1)
      }
