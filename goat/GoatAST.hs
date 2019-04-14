module GoatAST where

------------------------------------------------------------------------------
-- Identifier of Goat.
------------------------------------------------------------------------------
type Identifier = String

------------------------------------------------------------------------------
-- Base type of Goat.
------------------------------------------------------------------------------
data BaseType = BoolType | IntType | FloatType deriving (Show, Eq)

------------------------------------------------------------------------------
-- Variable of Goat.
------------------------------------------------------------------------------
data Variable = Variable { varId             :: Identifier
                         , varShapeIndicator :: ShapeIndicator
                         } deriving (Show, Eq)


data Expression = ExprVar { exprVar          :: Variable }
          | BoolConst { boolConstVal     :: Bool }
          | IntConst { intConstVal      :: Int }
          | FloatConst { floatConstVal    :: Float }
          | StrConst { strConstVal      :: String }
          | Add { addLeftExpr      :: Expression
                , addRightExpr     :: Expression
                }
          | Mul { mulLeftExpr      :: Expression
                , mulRightExpr     :: Expression
                }
          | Sub { subLeftExpr      :: Expression
                , subRightExpr     :: Expression
                }
    | Div
        { divLeftExpr      :: Expression
        , divRightExpr     :: Expression
        }
    | Or
        { orLeftExpr       :: Expression
        , orRightExpr      :: Expression
        }
    | And
        { andLeftExpr      :: Expression
        , andRightExpr     :: Expression
        }
    | Eq
        { eqLeftExpr       :: Expression
        , eqRightExpr      :: Expression
        }
    | NotEq
        { notEqLeftExpr    :: Expression
        , notEqRightExpr   :: Expression
        }
    | Les
        { lesLeftExpr      :: Expression
        , lesRightExpr     :: Expression
        }
    | LesEq
        { lesEqLeftExpr    :: Expression
        , lesEqRightExpr   :: Expression
        }
    | Grt
        { grtLeftExpr      :: Expression
        , grtRightExpr     :: Expression
        }
    | GrtEq
        { grtEqLeftExpr    :: Expression
        , grtEqRightExpr   :: Expression
        }
    | UnaryMinus
        { unaryMinusExpr   :: Expression }
    | UnaryNot
        { unaryNotExpr     :: Expression }
    deriving (Show, Eq)

data VariableDeclaration = VariableDeclaration { declarationType     :: BaseType
                                               , declarationVariable :: Variable
                                               } deriving (Show, Eq)

data Stmt
    = Assign
        { assignVal        :: Variable
        , assignExpr       :: Expression
        }
    | Read
        { readVal          :: Variable }
    | Write
        { writeExpr        :: Expression }
    | Call
        { callIdent        :: Identifier
        , callExprs        :: [Expression]
        }
    | If
        { ifExpr           :: Expression
        , ifStmts          :: [Stmt]
        }
    | IfElse
        { ifElseExpr       :: Expression
        , ifElseStmts1     :: [Stmt]
        , ifElseStmts2     :: [Stmt]
        }
    | While
        { whileExpr        :: Expression
        , whileStmts       :: [Stmt]
        }
    deriving (Show, Eq)

data Body
    = Body
    { bodyVarDeclarations  :: [VariableDeclaration]
    , bodyStatements       :: [Stmt]
    } deriving (Show, Eq)

data PIndicator
    = VarType
    | RefType
    deriving (Show, Eq)

data ShapeIndicator
    = Array
      { arrayExpr          :: Expression }
    | Matrix
      { matrixMExpr        :: Expression
      , matrixNExpr        :: Expression
      }
    | NoIndicator
    deriving (Show, Eq)

data Parameter
    = Parameter
    { passingIndicator     :: PIndicator
    , passingType          :: BaseType
    , passingIdent         :: Identifier
    } deriving (Show, Eq)

data Header
    = Header
    { headerIdent          :: Identifier
    , parameters           :: [Parameter]
    } deriving (Show, Eq)

data Procedure
    = Procedure
    { header               :: Header
    , body                 :: Body
    } deriving (Show, Eq)

data GoatProgram
    = GoatProgram
    { procedures           :: [Procedure]
    } deriving (Show, Eq)
