module Util where

import           GoatAST
import           SymbolTable

-------------------------------- Documentation --------------------------------

-- Authors:
--   Shizhe Cai (shizhec) - 798125
--   Weizhuo Zhang (weizhuoz) - 1018329
--   Mingyang Zhang (mingyangz) - 650242
--   An Luo (aluo1) - 657605

-- This file contains the util functions used by multiple modules of the Goat
-- program.

-- The aim of the project is to implement a compiler for a procedural (C-like)
-- language called Goat.

-------------------------------- Documentation --------------------------------

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
