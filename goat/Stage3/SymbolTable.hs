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

data ProcedureTable =
  ProcedureTable { param :: ParameterMap
                 , vari :: VariableMap
                 , statements :: [Statement]
                 } deriving (Show, Eq)
