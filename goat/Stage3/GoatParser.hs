module GoatParser where

import           Data.Functor.Identity (Identity)
import           GoatAST
import           GoatConstant
import           Text.Parsec
import           Text.Parsec.Expr
import           Text.Parsec.Language  (emptyDef)
import qualified Text.Parsec.Token     as Q

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
myOperators = [addSymbol, minusSymbol, timesSymbol, lessThanSymbol,
              greaterThanSymbol, lessThanOrEqualSymbol,
              greaterThanOrEqualSymbol, equalSymbol, notEqualSymbol,
              "||", "&&" ,"!", divSymbol, ":="]

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
    pIndicator    <-  pParameterIndicator
    parameterType <-  pParameterType
    id            <-  identifier
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
    statements           <- many1 pStatement
    reserved "end"
    return (Body variableDeclarations statements)
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
    try (do { (intM, intN) <- brackets $ pMatrix IntMatrix
            ; return (Matrix intM intN)
            }
        )
    <|>  do { return (NoIndicator) }
    <?> "shape indicator"


-------------------------------------------------------------------------------
-- pMatrix looks for the 2 dimensional matrix
-------------------------------------------------------------------------------
data MatrixType = IntMatrix | ExpressionMatrix
pMatrix :: MatrixType -> Parser (Expression, Expression)
pMatrix IntMatrix = do
    intM <- pInt
    comma
    intN <- pInt
    return (intM, intN)
pMatrix ExpressionMatrix = do
    expressionM <- pExpression
    comma
    expressionN <- pExpression
    return (expressionM, expressionN)

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
    expression <- (pString <|> pExpression)
    whiteSpace
    semi
    return (Write expression)
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
    whiteSpace
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
    expressionList <- parens $ sepBy pExpression comma
    whiteSpace
    semi
    return (Call id expressionList)
    <?> "Call statement"

-------------------------------------------------------------------------------
-- Define the if statement.
-------------------------------------------------------------------------------
pIf :: Parser Statement
pIf =
    try(do { reserved "if"
           ; expression <- pExpression
           ; reserved "then"
           ; statements <- many1 pStatement
           ; reserved "fi"
           ; return (If expression statements)
           }
        )
    <|> do { reserved "if"
           ; expression     <- pExpression
           ; reserved "then"
           ; ifStatements   <- many1 pStatement
           ; reserved "else"
           ; elseStatements <- many1 pStatement
           ; reserved "fi"
           ; return (IfElse expression ifStatements elseStatements)
           }
    <?> "If statement"

-------------------------------------------------------------------------------
-- Define the while statement
-------------------------------------------------------------------------------
pWhile :: Parser Statement
pWhile = do
    reserved "while"
    expression <- pExpression
    reserved "do"
    statements <- many1 pStatement
    reserved "od"
    return (While expression statements)
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

-------------------------------------------------------------------------------
-- Define the operator precedence table used in buildExpressionParser.
-- The operator in the top of the table has the highest precedence, and the one
-- at the bottom has the lowest precedence.
-------------------------------------------------------------------------------
table :: [[Operator String Int Identity Expression]]
table = [[prefix   minusSymbol UnaryMinus]
        ,[binary   timesSymbol Mul, binary   divSymbol  Div]
        ,[binary   addSymbol Add, binary   minusSymbol  Sub]
        ,[relation equalSymbol Eq,  relation notEqualSymbol NotEq
        , relation lessThanSymbol Les, relation lessThanOrEqualSymbol LesEq
        , relation greaterThanSymbol Grt
        , relation greaterThanOrEqualSymbol GrtEq]
        ,[prefix   "!" UnaryNot]
        ,[binary   "&&" And]
        ,[binary   "||" Or]]

-------------------------------------------------------------------------------
-- Prefix operator
-------------------------------------------------------------------------------
prefix :: String -> (Expression -> Expression)
       -> Operator String Int Identity Expression
prefix name prefixOperator = Prefix (do { reservedOp name;
                                          return prefixOperator
                                        })

-------------------------------------------------------------------------------
-- Binary operator
-------------------------------------------------------------------------------
binary :: String -> (Expression -> Expression -> Expression)
       -> Operator String Int Identity Expression
binary name binaryOperator = Infix (do { reservedOp name;
                                         return binaryOperator
                                       }
                                   ) AssocLeft

-------------------------------------------------------------------------------
-- Relation operator.
-------------------------------------------------------------------------------
relation :: String -> (Expression -> Expression -> Expression)
         -> Operator String Int Identity Expression
relation name relationOperator = Infix (do { reservedOp name;
                                             return relationOperator
                                             }
                                       ) AssocNone

-------------------------------------------------------------------------------
-- Define parser parsing float number.
-------------------------------------------------------------------------------
pFloat :: Parser Expression
pFloat = do { n <- many1 digit;
              char '.';
              m <- many1 digit;
              whiteSpace;
              return (FloatConst (read (n ++ "." ++m) :: Float))
            }
         <?> "float"

-------------------------------------------------------------------------------
-- Define parser parsing integer.
-------------------------------------------------------------------------------
pInt :: Parser Expression
pInt = do { n <- natural;
            return (IntConst (fromInteger n :: Int))
          }
       <?> "integer"

-------------------------------------------------------------------------------
-- Define parser parsing the shape indicator.
-------------------------------------------------------------------------------
pExpressionShapeIndicator :: Parser ShapeIndicator
pExpressionShapeIndicator =
    try (do { expression <- brackets pExpression
            ; return (Array expression)
            }
        )
    <|>
    try (do { (expressionM, expressionN) <- brackets $ pMatrix ExpressionMatrix
            ; return (Matrix expressionM expressionN)
            }
        )
    <|>  do { return (NoIndicator) }

-------------------------------------------------------------------------------
-- Define parser parsing the identifier.
-------------------------------------------------------------------------------
pIdentifier :: Parser Expression
pIdentifier = do
  id             <- identifier
  shapeIndicator <- pExpressionShapeIndicator
  return (ExprVar (Variable id shapeIndicator))
  <?> "identifier"

-------------------------------------------------------------------------------
-- Define parser parsing the string.
-------------------------------------------------------------------------------
pString :: Parser Expression
pString = do
  char '"'
  str <- many (satisfy (/= '"'))
  char '"'
  return (StrConst str)
  <?> "string"

-------------------------------------------------------------------------------
-- Define parser parsing the bool value.
-------------------------------------------------------------------------------
pBool :: Parser Expression
pBool = do {reserved "true"; return (BoolConst True)}
        <|>
        do {reserved "false"; return (BoolConst False)}
        <?> "bool"

-------------------------------------------------------------------------------
-- main function of the parser.
-------------------------------------------------------------------------------
pMain :: Parser GoatProgram
pMain = do
  whiteSpace
  p <- pProgram
  eof
  return p
