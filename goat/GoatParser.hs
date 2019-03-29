module Main where

import GoatAST
import Data.Char
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Q
import System.Environment
import System.Exit

type Parser a
    = Parsec String Int a

lexer :: Q.TokenParser Int
lexer
  = Q.makeTokenParser
    (emptyDef
    { Q.commentLine    = "#"
    , Q.nestedComments = True
    , Q.identStart     = letter
    , Q.opStart         = oneOf "+-*:"
    , Q.opLetter        = oneOf "+-*:"
    , Q.reservedNames   = myReserved
    , Q.reservedOpNames = myOpnames
    })

whiteSpace = Q.whiteSpace lexer
lexeme     = Q.lexeme lexer
natural    = Q.natural lexer
identifier = Q.identifier lexer
colon      = Q.colon lexer
semi       = Q.semi lexer
comma      = Q.comma lexer
parens     = Q.parens lexer
squares    = Q.squares lexer
reserved   = Q.reserved lexer
reservedOp = Q.reservedOp lexer

myReserved
  = ["begin", "bool", "do", "else", "end",
  "false", "fi", "float", "if", "int", "od",
     "proc", "read", "ref", "then", "true", "val",
     "while", "write"]

myOpnames
  = ["+", "-", "*", ":=", "<", ">", "<=", ">=", "=", "!=",
    "||", "&&", "!", "/"]




pProg :: Parser GoatProgram
pProg
  = do
      (header,body) <- many1 pProcedure
      return (Program header body)

pProcedure :: Parser (Header,Body)
pProcedure
  = do
    reserved "proc"
    header    <- pProgHeader
    reserved "begin"
    body      <- pProgBody
    reserved "end"
    return (header,body)

pProgHeader :: Parser (Ident,Parameter)
pProgHeader
  = do
    ident     <- identifier
    char "("
    params    <- sepby pParameter comma
    char ")"
    return (ident,params)

pParameter :: Parser (Pindicator,Ptype,Ident)
pParameter
  = do
    pidcat    <-  pPindicator
    ptype     <-  pPtype
    ident     <-  identifier
    return (pidcat,ptype,ident)

pPindicator :: Parser Pindicator
pPindicator
  = do { reserved "var"; return VarType }
    <|>
    do { reserved "ref"; return RefType }

pPtype :: Parser Ptype
pPtype
  = do { reserved "bool"; return BoolType }
    <|>
    do { reserved "int"; return IntType }
    <|>
    do { reserved "float"; return FloatType }

pProgBody :: Parser (VDecl,Stmt)
pProgBody
  = do
    vdecls <- many   pVDecl
    stmts  <- many1  pStmt
    return (vdecls,stmts)

pVDecl :: Parser VDecl
pVDecl
  = do
    ptype   <-   pPtype
    ident   <-   identifier
    sidcat  <- optional pSindicator
    semi
    return (ptype,ident,sidcat)

pSindicator :: Parser Sindicator
pSindicator
  = do { char "["
          n <- pNum
          comma
          m <- pNum
         char "]" ; return (Matrix (n,m))}
    <|>
    do {
        char "["
        n <- pNum
        char "]" ; return (Array n)
      }

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

-- the assig operation, need to modify
pAsg
  = do
      lvalue <- pLvalue
      reservedOp ":="
      rvalue <- pExp
      semi
      return (Assign lvalue rvalue)

pCall
  = do
      lvalue <- pLvalue
      explist <- sepby pExp comma <|> return ()
      return (Call lvalue explist)

pIf
  = do
      reserved "if"
      exp <- pExp
      reserved "then"
      stmts <- many1 pStmt
      semi
      reserved "fi"
      return (If exp stmts)

pIfElse
  = do
      reserved "if"
      exp <- pExp
      reserved "then"
      stmts1 <- many1 pStmt
      semi
      reserved "else"
      stmts2 <- many1 pStmt
      semi
      reserved "if"
      return (IfElse exp stmts1 stmts2)

pWhile
  = do
    reserved "while"
    exp <- pExp
    reserved "do"
    stmts <- many1 pStmt
    semi
    reserved "od"
    return (While exp stmts)

pExp, pTerm, pFactor, pNum, pIdent, pString :: Parser Expr

pExp
  = pString <|> (chainl1 pTerm pAddOp)
    <?>
    "expression"

pTerm
  = chainl1 pFactor pMulOp
    <?>
    "\"term\""

pFactor
  = choice [pUneg, pUnot, parsens pExp, pNum, pIdent]
    <?>
    "\"factor\""

pNum
  = do
      n <- natural <?> ""
      return (IntConst (fromInteger n :: Int))
      <?>
      "number"

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

pUneg, pUnot :: Parsec Unaryop

pUneg
  = do
      reservedOp "-"
      exp <- pFactor
      return (Op_neg exp)

pUnot
  = do
      reservedOp "!"
      exp <- pFactor
      return (Op_not exp)
