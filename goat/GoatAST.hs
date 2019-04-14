module GoatAST where

-------------------------------------------------------------------------------
-- Identifier.
-------------------------------------------------------------------------------
type Identifier = String

-------------------------------------------------------------------------------
-- Base type.
-------------------------------------------------------------------------------
data BaseType = BoolType | IntType | FloatType deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Variable.
-------------------------------------------------------------------------------
data Variable = Variable { varId             :: Identifier
                         , varShapeIndicator :: ShapeIndicator
                         } deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Expression.
-------------------------------------------------------------------------------
data Expression = ExprVar { exprVar          :: Variable }
                | BoolConst { boolConstVal     :: Bool }
                | IntConst { intConstVal      :: Int }
                | FloatConst { floatConstVal    :: Float }
                | StrConst { strConstVal      :: String }
                | Add { addLeftExpr  :: Expression
                      , addRightExpr :: Expression
                      }
                | Mul { mulLeftExpr  :: Expression
                      , mulRightExpr :: Expression
                      }
                | Sub { subLeftExpr  :: Expression
                      , subRightExpr :: Expression
                      }
                | Div { divLeftExpr  :: Expression
                      , divRightExpr :: Expression
                      }
                | Or { orLeftExpr  :: Expression
                     , orRightExpr :: Expression
                     }
                | And { andLeftExpr  :: Expression
                      , andRightExpr :: Expression
                      }
                | Eq { eqLeftExpr  :: Expression
                     , eqRightExpr :: Expression
                     }
                | NotEq { notEqLeftExpr  :: Expression
                        , notEqRightExpr :: Expression
                        }
                | Les { lesLeftExpr  :: Expression
                      , lesRightExpr :: Expression
                      }
                | LesEq { lesEqLeftExpr  :: Expression
                        , lesEqRightExpr :: Expression
                        }
                | Grt { grtLeftExpr  :: Expression
                      , grtRightExpr :: Expression
                      }
                | GrtEq { grtEqLeftExpr  :: Expression
                        , grtEqRightExpr :: Expression
                        }
                | UnaryMinus { unaryMinusExpr :: Expression }
                | UnaryNot { unaryNotExpr :: Expression }
                deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Variable Declaration.
-------------------------------------------------------------------------------
data VariableDeclaration = VariableDeclaration
                           { declarationType     :: BaseType
                           , declarationVariable :: Variable
                           } deriving (Show, Eq)

 ------------------------------------------------------------------------------
 -- Statement types.
 ------------------------------------------------------------------------------
data Statement = Assign { assignVal  :: Variable
                        , assignExpr :: Expression
                        }
               | Read { readVal          :: Variable }
               | Write { writeExpr        :: Expression }
               | Call { callIdent :: Identifier
                      , callExprs :: [Expression]
                      }
               | If { ifExpr       :: Expression
                    , ifStatements :: [Statement]
                    }
               | IfElse { ifElseExpr        :: Expression
                        , ifElseStatements1 :: [Statement]
                        , ifElseStatements2 :: [Statement]
                        }
               | While { whileExpr       :: Expression
                       , whileStatements :: [Statement]
                       }
               deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Procedure body.
-------------------------------------------------------------------------------
data Body = Body { bodyVarDeclarations :: [VariableDeclaration]
                 , bodyStatements      :: [Statement]
                 } deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Procedure indicator types.
-------------------------------------------------------------------------------
data PIndicator = VarType | RefType deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Shape indicator types.
-------------------------------------------------------------------------------
data ShapeIndicator = Array { arrayExpr :: Expression }
                    | Matrix { matrixMExpr :: Expression
                             , matrixNExpr :: Expression
                             }
                    | NoIndicator
                    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Procedure parameter types.
-------------------------------------------------------------------------------
data Parameter = Parameter { passingIndicator :: PIndicator
                           , passingType      :: BaseType
                           , passingIdent     :: Identifier
                           } deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Procedure header.
-------------------------------------------------------------------------------
data Header = Header { headerIdent :: Identifier
                     , parameters  :: [Parameter]
                     } deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Procedure.
-------------------------------------------------------------------------------
data Procedure = Procedure { header :: Header
                           , body   :: Body
                           } deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Goat program types.
-------------------------------------------------------------------------------
data GoatProgram = GoatProgram { procedures :: [Procedure]} deriving (Show, Eq)
