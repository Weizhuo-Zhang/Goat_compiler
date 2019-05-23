module SymbolTable where

import Data.Map
import qualified Data.Map.Strict as M
import GoatAST

-------------------------------- Documentation --------------------------------

-- Authors:
--   Shizhe Cai (shizhec) - 798125
--   Weizhuo Zhang (weizhuoz) - 1018329
--   Mingyang Zhang (mingyangz) - 650242
--   An Luo (aluo1) - 657605

-- This file contains the symbol table-related information of the Goat program.

-- The aim of the project is to implement a compiler for a procedural (C-like)
-- language called Goat.

-------------------------------- Documentation --------------------------------

type ProgramMap = M.Map Identifier ProcedureTable

type ParameterMap = M.Map Identifier Parameter

type VariableMap = M.Map Identifier VariableDeclaration

data ProcedureTable = ProcedureTable { parameterMap :: ParameterMap
                                     , variableMap :: VariableMap
                                     , statementTable :: [StatementTable]
                                     } deriving (Show, Eq)

data StatementTable = StatementTable { statement :: Statement
                                     , expressionTable :: ExpressionTable
                                     } deriving (Show, Eq)

data ExpressionTable = ExpressionTable { expression     :: Expression
                                       , expressionType :: BaseType
                                       }
                     | StringTable     { stringVal :: String }
                     | IntTable        { intVal    :: Int }
                     | FloatTable      { floatVal  :: Float }
                     | BoolTable       { boolVal   :: Bool }
                     deriving (Show, Eq)
