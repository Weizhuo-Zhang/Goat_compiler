module GoatParser where

import GoatAST
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Q

-------------------------------- Documentation --------------------------------

-- Authors:
--   Shizhe Cai (shizhec) - 798125
--   Weizhuo Zhang (weizhuoz) - 1018329
--   Mingyang Zhang (mingyangz) - 650242
--   An Luo (aluo1) - 657605

-- This file contains the parser-related information of the Goat program.

-- The aim of the project is to implement a compiler for a procedural (C-like)
-- language called Goat.

-------------------------------- Documentation --------------------------------

-------------------------------------------------------------------------------
-- Define the synonym for of Parser type.
-------------------------------------------------------------------------------
type Parser a = Parsec String Int a

-------------------------------------------------------------------------------
-- lexer definition
-------------------------------------------------------------------------------
lexer :: Q.TokenParser Int
lexer = Q.makeTokenParser (emptyDef { Q.commentLine     = "#"
                                    , Q.nestedComments  = True
                                    , Q.identStart      = letter
                                    , Q.identLetter     = alphaNum
                                                       <|> char '_'
                                                       <|> char '\''
                                    , Q.opStart         = oneOf "+-*/|&!=<>:"
                                    , Q.opLetter        = oneOf "|&="
                                    , Q.reservedNames   = myReservedWords
                                    , Q.reservedOpNames = myOperators
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

-------------------------------------------------------------------------------
-- Define reserved words.
-------------------------------------------------------------------------------
myReservedWords :: [String]
myReservedWords = ["begin", "bool",  "call", "do",  "else",  "end",  "false"
                  ,"fi"   , "float", "if"  , "int", "od",    "proc", "read"
                  ,"ref"  , "then",  "true", "val", "while", "write"]

-------------------------------------------------------------------------------
-- Define reserved operator
-------------------------------------------------------------------------------
myOperators :: [String]
myOperators = ["+", "-", "*", "<", ">", "<=", ">=", "=", "!=", "||", "&&" ,"!"
              ,"/", ":="]

-------------------------------------------------------------------------------
-- This is the top-most parsing function. It looks for a program which contains
-- one or more procedures.
-------------------------------------------------------------------------------
pProgram :: Parser GoatProgram
pProgram = do
    procedures <- many1 pProcedure
    return (GoatProgram procedures)
    <?> "program procedure"

-------------------------------------------------------------------------------
-- pProcedure looks for a procedure, whose structure should be:
-- "proc" + header + "begin" + body + "end"
-------------------------------------------------------------------------------
pProcedure :: Parser Procedure
pProcedure = do
    reserved "proc"
    header    <- pProcedureHeader
    body      <- pProcedureBody
    return (Procedure header body)
    <?> "procedure"

-------------------------------------------------------------------------------
-- pProcedureHeader looks for the procedure header, which contains a
-- procedure name followed by serveral parameters and variable declarations.
-------------------------------------------------------------------------------
pProcedureHeader :: Parser Header
pProcedureHeader = do
    id     <- identifier
    params <- parens $ sepBy pParameter comma
    return (Header id params)
    <?> "procedure header"

-------------------------------------------------------------------------------
-- pParameter looks for the parameter, which contains an indicator, type of the
-- parameter, and the identifier.
-------------------------------------------------------------------------------
pParameter :: Parser Parameter
pParameter = do
    pIndicator <-  pParameterIndicator
    parameterType      <-  pParameterType
    id         <-  identifier
    return (Parameter pIndicator parameterType id)
    <?> "parameters"


-------------------------------------------------------------------------------
-- pParameterIndicator looks for the indicator of the parameter, and return the
-- corresponding ParameterIndicator type value if it matches the value.
-------------------------------------------------------------------------------
pParameterIndicator :: Parser ParameterIndicator
pParameterIndicator = do { reserved "val"; return VarType }
                      <|>
                      do { reserved "ref"; return RefType }
                      <?> "passing indicator type"


-------------------------------------------------------------------------------
-- pParameterType looks for the base type of the parameter, and return the
-- corresponding BaseType type value if it matches.
-------------------------------------------------------------------------------
pParameterType :: Parser BaseType
pParameterType = do { reserved "bool"; return BoolType }
                 <|>
                 do { reserved "int"; return IntType }
                 <|>
                 do { reserved "float"; return FloatType }
                 <?> "base type indicator"

-------------------------------------------------------------------------------
-- pProcedureBody looks for body, which contains one or more statements.
-------------------------------------------------------------------------------
pProcedureBody :: Parser Body
pProcedureBody = do
    variableDeclarations <- many pVariableDeclaration
    reserved "begin"
    stmts                <- many1 pStatement
    reserved "end"
    return (Body variableDeclarations stmts)
    <?> "procedure body"

-------------------------------------------------------------------------------
-- pVariableDeclaration looks for the variable declaration, whose pattern
-- should be:
-- variableDeclaration := ParameterType identifier ShapeIndicator
-- If it is found, return a value with VariableDeclaration type.
-------------------------------------------------------------------------------
pVariableDeclaration :: Parser VariableDeclaration
pVariableDeclaration = do
    parameterType  <- pParameterType
    id             <- identifier
    shapeIndicator <- pShapeIndicator
    whiteSpace
    semi
    return (VariableDeclaration parameterType (Variable id shapeIndicator))
    <?> "procedure variable declaration"


-------------------------------------------------------------------------------
-- pShapeIndicator looks for the shape indicator of the variable, whose pattern
-- should be: [m] or [m, n].
-------------------------------------------------------------------------------
pShapeIndicator :: Parser ShapeIndicator
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

-------------------------------------------------------------------------------
-- Define statement.
-------------------------------------------------------------------------------
pStatement :: Parser Statement
pStatement = choice [pAssignment, pRead, pWrite, pCall, pIf, pWhile]
          <?> "statement"

-------------------------------------------------------------------------------
-- Define the read statement.
-------------------------------------------------------------------------------
pRead :: Parser Statement
pRead = do
    reserved "read"
    id             <- identifier
    shapeIndicator <- pExpressionShapeIndicator
    semi
    return (Read (Variable id shapeIndicator))
    <?> "read statement"

-------------------------------------------------------------------------------
-- Define the write statement.
-------------------------------------------------------------------------------
pWrite :: Parser Statement
pWrite = do
    reserved "write"
    exp <- (pString <|> pExpression)
    semi
    return (Write exp)
    <?> "write statement"

-------------------------------------------------------------------------------
-- Define the assignment statement.
-------------------------------------------------------------------------------
pAssignment :: Parser Statement
pAssignment = do
    id  <- identifier
    shapeIndicator <- pExpressionShapeIndicator
    whiteSpace
    reservedOp ":="
    rvalue <- pExpression
    semi
    return (Assign (Variable id shapeIndicator) rvalue)
    <?> "Assign statement"

-------------------------------------------------------------------------------
-- Define the call statement.
-------------------------------------------------------------------------------
pCall :: Parser Statement
pCall = do
    reserved "call"
    id      <- identifier
    expList <- parens $ sepBy pExpression comma
    semi
    return (Call id expList)
    <?> "Call statement"

-------------------------------------------------------------------------------
-- Define the if statement.
-------------------------------------------------------------------------------
pIf :: Parser Statement
pIf =
    try( do
        { reserved "if"
        ; exp    <- pExpression
        ; reserved "then"
        ; stmts  <- many1 pStatement
        ; reserved "fi"
        ; return (If exp stmts)
        })
    <|> do
        { reserved "if"
        ; exp    <- pExpression
        ; reserved "then"
        ; stmts1 <- many1 pStatement
        ; reserved "else"
        ; stmts2 <- many1 pStatement
        ; reserved "fi"
        ; return (IfElse exp stmts1 stmts2)
        }
    <?> "If statement"

-------------------------------------------------------------------------------
-- Define the while statement
-------------------------------------------------------------------------------
pWhile :: Parser Statement
pWhile = do
    reserved "while"
    exp   <- pExpression
    reserved "do"
    stmts <- many1 pStatement
    reserved "od"
    return (While exp stmts)
    <?> "While statement"

-------------------------------------------------------------------------------
-- define expressions
-- expression contains operations, relations, expressions, string
-- and boolean
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Define the expression.
-------------------------------------------------------------------------------
pExpression :: Parser Expression
pExpression = buildExpressionParser table pTerm

-------------------------------------------------------------------------------
-- Define the term used in buildExpressionParser.
-------------------------------------------------------------------------------
pTerm :: Parser Expression
pTerm = choice [parens pExpression, pNumber, pIdentifier, pBool]


-------------------------------------------------------------------------------
-- Define the number.
-------------------------------------------------------------------------------
pNumber :: Parser Expression
pNumber = try (do pFloat) <|> do pInt


table = [[prefix   "-" UnaryMinus]
        ,[binary   "*" Mul, binary   "/"  Div]
        ,[binary   "+" Add, binary   "-"  Sub]
        ,[relation "=" Eq,  relation "!=" NotEq
        , relation "<" Les, relation "<=" LesEq
        , relation ">" Grt, relation ">=" GrtEq]
        ,[prefix   "!" UnaryNot]
        ,[binary   "&&" And]
        ,[binary   "||" Or]]

prefix name func = Prefix (do { reservedOp name; return func })

binary name op = Infix (do { reservedOp name; return op }) AssocLeft

relation name rel = Infix (do { reservedOp name; return rel }) AssocNone

pFloat :: Parser Expression
pFloat = do { n <- many1 digit;
              char '.';
              m <- many1 digit;
              return (FloatConst (read (n ++ "." ++m) :: Float))
            }
         <?> "float"

pInt :: Parser Expression
pInt = do { n <- natural;
            return (IntConst (fromInteger n :: Int))
          }
       <?> "integer"

pExpressionShapeIndicator :: Parser ShapeIndicator
pExpressionShapeIndicator =
    try (do { exp <- brackets pExpression
            ; return (Array exp)
            }
        )
    <|>
    try (do { expList <- brackets $ sepBy pExpression comma
            ; return (Matrix (expList !! 0) (expList !! 1))
            }
        )
    <|>  do { return (NoIndicator) }

pIdentifier = do
  id <- identifier
  shapeIndicator <- pExpressionShapeIndicator
  return (ExprVar (Variable id shapeIndicator))
  <?>
  "identifier"

pString :: Parser Expression
pString = do
  char '"'
  str <- many (satisfy (/= '"'))
  char '"'
  return (StrConst str)
  <?>
  "string"

pBool :: Parser Expression
pBool = do {reserved "true"; return (BoolConst True)}
        <|>
        do {reserved "false"; return (BoolConst False)}
        <?> "bool"

pMain :: Parser GoatProgram
pMain = do
  whiteSpace
  p <- pProgram
  eof
  return p
