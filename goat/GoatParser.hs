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
lexer = Q.makeTokenParser (emptyDef { Q.commentLine     = "#"
                                    , Q.nestedComments  = True
                                    , Q.identStart      = letter
                                    , Q.identLetter     = alphaNum
                                                       <|> char '_'
                                                       <|> char '\''
                                    , Q.opStart         = oneOf "+-*/|&!=<>:"
                                    , Q.opLetter        = oneOf "|&="
                                    , Q.reservedNames   = myReserved
                                    , Q.reservedOpNames = myOpnames
                                    }
                          )

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
brackets   = Q.brackets lexer
reserved   = Q.reserved lexer
reservedOp = Q.reservedOp lexer

myReserved, myOpnames :: [String]

myReserved = ["begin", "bool", "call" , "do"  , "else", "end", "false", "fi"
             ,"float", "if"  , "int" , "od", "proc" , "read", "ref"  , "then"
             , "true", "val", "while", "write"]

myOpnames = ["+" , "-" , "*", "<", ">", "<=", ">=", "=", "!=", "||", "&&"
            , "!", "/", ":="]

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
    header    <- pProcedureHeader
    body      <- pProcedureBody
    return (Procedure header body)
    <?> "procedure"

-----------------------------------------------------------------
-- pProcedureHeader looks for the program header, which contains a
-- function name followed by serveral parameters and variable
-- declarations
-----------------------------------------------------------------
pProcedureHeader :: Parser Header
pProcedureHeader = do
    id     <- identifier
    params <- parens $ sepBy pParameter comma
    return (Header ident params)
    <?> "procedure header"

-----------------------------------------------------------------
-- parameters := (val|ref) (int|float|bool) identifier
-----------------------------------------------------------------

pParameter :: Parser Parameter
pParameter = do
    pIndicator    <-  pPIndicator
    pType         <-  pPtype
    id            <-  identifier
    return (Parameter pIndicator pType id)
    <?> "parameters"


pPIndicator :: Parser PIndicator
pPIndicator
  = do { reserved "val"; return VarType }
    <|>
    do { reserved "ref"; return RefType }
    <?> "passing indicator type"


pPtype :: Parser BaseType
pPtype
  = do { reserved "bool"; return BoolType }
    <|>
    do { reserved "int"; return IntType }
    <|>
    do { reserved "float"; return FloatType }
    <?> "base type indicator"

-----------------------------------------------------------------
-- pProcedureBody looks for body, which contains one or more statements
-----------------------------------------------------------------

pProcedureBody :: Parser Body
pProcedureBody = do
    variableDeclarations <- many pVariableDeclaration
    reserved "begin"
    stmts                <- many1 pStatement
    reserved "end"
    return (Body variableDeclarations stmts)
    <?> "procedure body"

-----------------------------------------------------------------
-- variable declarations
-- vdecl := (int|float|bool) identifier (shape indicator)
-- shape indicator
-- determine whether a identifier is an array, a matrix or
-- no indicator
-----------------------------------------------------------------

pVariableDeclaration :: Parser VariableDeclaration
pVariableDeclaration = do
    pType  <- pPtype
    id  <- identifier
    shapeIndicator <- pShapeIndicator
    whiteSpace
    semi
    return (VariableDeclaration pType (Variable id shapeIndicator))
    <?> "procedure variable declaration"


pShapeIndicator :: Parser SIndicator
pShapeIndicator =
    try (do { n <- brackets pInt
            ; return (Array n)
            }
        )
    <|>
    try (do { intList <- brackets $ sepBy pInt comma
            ; return (Matrix (intList !! 0) (intList !! 1))
            }
        )
    <|>  do { return (NoIndicator) }
    <?> "shape indicator"

-----------------------------------------------------------------
-- define statements
-- statement contains read, asgin, write, call, if, ifelse, while
-----------------------------------------------------------------

pStatement, pAssignment, pRead, pWrite, pCall, pIf, pWhile :: Parser Stmt

pStatement = choice [pAssignment, pRead, pWrite, pCall, pIf, pWhile]
          <?> "statement"

pRead = do
    reserved "read"
    id  <- identifier
    shapeIndicator <- pExprSIndicator
    semi
    return (Read (Variable id shapeIndicator))
    <?> "read statement"

pWrite = do
    reserved "write"
    exp <- (pString <|> pExp)
    semi
    return (Write exp)
    <?> "write statement"

pAssignment = do
    id  <- identifier
    shapeIndicator <- pExprSIndicator
    whiteSpace
    reservedOp ":="
    rvalue <- pExp
    semi
    return (Assign (Variable id shapeIndicator) rvalue)
    <?> "Assign statement"

pCall = do
    reserved "call"
    id   <- identifier
    expList <- parens $ sepBy pExp comma
    semi
    return (Call id expList)
    <?> "Call statement"

pIf =
    try( do
        { reserved "if"
        ; exp   <- pExp
        ; reserved "then"
        ; stmts <- many1 pStatement
        ; reserved "fi"
        ; return (If exp stmts)
        })
    <|> do
        { reserved "if"
        ; exp    <- pExp
        ; reserved "then"
        ; stmts1 <- many1 pStatement
        ; reserved "else"
        ; stmts2 <- many1 pStatement
        ; reserved "fi"
        ; return (IfElse exp stmts1 stmts2)
        }
    <?> "If statement"

pWhile = do
    reserved "while"
    exp   <- pExp
    reserved "do"
    stmts <- many1 pStatement
    reserved "od"
    return (While exp stmts)
    <?> "While statement"

-----------------------------------------------------------------
-- define expressions
-- expression contains operations, relations, expressions, string
-- and boolean
-----------------------------------------------------------------
pExp, pFloat, pInt, pIdent, pString, pBool :: Parser Expression

pExp
 = buildExpressionParser table pFac

pFac :: Parser Expression
pFac = choice [parens pExp, pNum, pIdent, pBool]

pNum :: Parser Expression
pNum = try (do pFloat)
      <|>
      do pInt


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

pFloat =
      do { n <- many1 digit;
           char '.';
           m <- many1 digit;
           return (FloatConst (read (n ++ "." ++m) :: Float))
         }
        <?> "float"
pInt =
      do { n <- natural;
           return (IntConst (fromInteger n :: Int))
         }
        <?> "integer"

pExprSIndicator :: Parser SIndicator
pExprSIndicator =
    try (do { exp <- brackets pExp
            ; return (Array exp)
            }
        )
    <|>
    try (do { expList <- brackets $ sepBy pExp comma
            ; return (Matrix (expList !! 0) (expList !! 1))
            }
        )
    <|>  do { return (NoIndicator) }

pIdent
  = do
      id <- identifier
      shapeIndicator <- pExprSIndicator
      return (ExprVar (Variable id shapeIndicator))
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
