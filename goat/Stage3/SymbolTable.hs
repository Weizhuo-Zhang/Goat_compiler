module SymbolTable where

import           Data.Map
import qualified Data.Map.Strict as M
import           GoatAST

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

type ParameterMap = M.Map Identifier (Int, Parameter)

type VariableMap = M.Map Identifier VariableDeclaration

type StackMap = M.Map Identifier Int

data ProcedureTable = ProcedureTable { parameterMap   :: ParameterMap
                                     , variableMap    :: VariableMap
                                     , statementTable :: [StatementTable]
                                     } deriving (Show, Eq)

data StatementTable = WriteTable     { writeExprTable    :: ExpressionTable }
                    | ReadTable      { readExprTable     :: ExpressionTable }
                    | AssignTable    { assignVarTable  :: ExpressionTable
                                     , assignExprTable :: ExpressionTable}
                    | IfTable        { ifExprTable  :: ExpressionTable
                                     , ifStmtTables :: [StatementTable]
                                     }
                    | IfElseTable    { ifElseExprTable   :: ExpressionTable
                                     , ifElseStmtTables1 :: [StatementTable]
                                     , ifElseStmtTables2 :: [StatementTable]
                                     }
                    | WhileTable     { whileExprTable  :: ExpressionTable
                                     , whileStmtTables :: [StatementTable]
                                     } 
                    | CallTable      { procIdentifier :: Identifier
                                     , argExprTables  :: [ExpressionTable]
                                     , callParamList  :: [Parameter]
                                     } deriving (Show, Eq)

data VariableSubTable = VariableSubTable { varName                :: Identifier
                                         , varShapeIndicatorTable :: ShapeIndicatorTable
                                         } deriving (Show, Eq)

data ShapeIndicatorTable = ArrayTable  { arrayExprTable   :: ExpressionTable }
                         | MatrixTable { matrixMExprTable :: ExpressionTable
                                       , matrixNExprTable :: ExpressionTable
                                       }
                         | NoIndicatorTable
                         deriving (Show, Eq)

data ExpressionTable = VariableTable   { variable     :: VariableSubTable
                                       , variableType :: BaseType
                                       }
                     | BoolTable       { boolVal   :: Bool }
                     | IntTable        { intVal    :: Int }
                     | FloatTable      { floatVal  :: Float }
                     | StringTable     { stringVal :: String }
                     | AddTable        { addLeftVal  :: ExpressionTable
                                       , addRightVal :: ExpressionTable
                                       , addType     :: BaseType
                                       }
                     | SubTable        { subLeftVal  :: ExpressionTable
                                       , subRightVal :: ExpressionTable
                                       , subType     :: BaseType
                                       }
                     | MulTable        { mulLeftVal  :: ExpressionTable
                                       , mulRightVal :: ExpressionTable
                                       , mulType     :: BaseType
                                       }
                     | DivTable        { divLeftVal  :: ExpressionTable
                                       , divRightVal :: ExpressionTable
                                       , divType     :: BaseType
                                       }
                     | OrTable         { orLeftExprTable  :: ExpressionTable
                                       , orRightExprTable :: ExpressionTable
                                       , orType           :: BaseType
                                       }
                     | AndTable        { andLeftExprTable  :: ExpressionTable
                                       , andRightExprTable :: ExpressionTable
                                       , andType           :: BaseType
                                       }
                     | EqTable         { eqLeftExpr  :: ExpressionTable
                                       , eqRightExpr :: ExpressionTable
                                       , eqType      :: BaseType
                                       }
                     | NotEqTable      { notEqLeftExpr  :: ExpressionTable
                                       , notEqRightExpr :: ExpressionTable
                                       , notEqType      :: BaseType
                                       }
                     | LesTable        { lesLeftExpr  :: ExpressionTable
                                       , lesRightExpr :: ExpressionTable
                                       , lesType      :: BaseType
                                       }
                     | LesEqTable      { lesEqLeftExpr  :: ExpressionTable
                                       , lesEqRightExpr :: ExpressionTable
                                       , lesEqType      :: BaseType
                                       }
                     | GrtTable        { grtLeftExpr  :: ExpressionTable
                                       , grtRightExpr :: ExpressionTable
                                       , grtType      :: BaseType
                                       }
                     | GrtEqTable      { grtEqLeftExpr  :: ExpressionTable
                                       , grtEqRightExpr :: ExpressionTable
                                       , grtEqType      :: BaseType
                                       }
                     | NegativeTable   { negativeExpr :: ExpressionTable
                                       , negativeType :: BaseType
                                       }
                     | NotTable        { notExprTable :: ExpressionTable
                                       , notType      :: BaseType
                                       } deriving (Show, Eq)
