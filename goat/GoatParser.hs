module GoatParser where

import GoatAST
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Q

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
    whiteSpace
    params    <- sepBy pParameter comma
    char ')'
    whiteSpace
    -- newline
    -- whiteSpace
    return (Header ident params)
    <?> "procedure header"

-----------------------------------------------------------------
-- parameters := (val|ref) (int|float|bool) identifier
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
  = do { reserved "val"; return VarType }
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
-- variable declarations
-- vdecl := (int|float|bool) identifier (shape indicator)
-- shape indicator
-- determine whether a identifier is an array, a matrix or
-- no indicator
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
            ; whiteSpace
            ; n <- pNum
            ; char ']'
            ; return (Array n)
            }
        )
    <|>
    try (do { char '['
            ; whiteSpace
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
-- statement contains read, asgin, write, call, if, ifelse, while
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
-- expression contains operations, relations, expressions, string
-- and boolean
-----------------------------------------------------------------
pExp, pNum, pIdent, pString, pBool :: Parser Expr

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

pMain :: Parser GoatProgram
pMain
  = do
    whiteSpace
    p <- pProg
    eof
    return p
