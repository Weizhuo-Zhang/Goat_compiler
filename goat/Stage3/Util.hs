module Util where

import           GoatAST
import           SymbolTable

getAssignBaseType :: ExpressionTable -> BaseType
getAssignBaseType exprTable =
  case exprTable of
    VariableTable _ exprType -> exprType
    BoolTable  _             -> BoolType
    IntTable   _             -> IntType
    FloatTable _             -> FloatType
    AddTable   _ _  exprType -> exprType
    SubTable   _ _  exprType -> exprType
    MulTable   _ _  exprType -> exprType
    DivTable   _ _  exprType -> exprType
    OrTable    _ _  exprType -> BoolType
    AndTable   _ _  exprType -> BoolType
    EqTable    _ _  exprType -> BoolType
    NotEqTable _ _  exprType -> BoolType
    LesTable   _ _  exprType -> BoolType
    LesEqTable _ _  exprType -> BoolType
    GrtTable   _ _  exprType -> BoolType
    GrtEqTable _ _  exprType -> BoolType
    NegativeTable _ exprType -> exprType
    NotTable      _ exprType -> BoolType

-------------------------------------------------------------------------------
-- Wrap the provided string with double quotations.
-------------------------------------------------------------------------------
wrapWithDoubleQuotations :: String -> String
wrapWithDoubleQuotations value = "\"" ++ value ++ "\""
