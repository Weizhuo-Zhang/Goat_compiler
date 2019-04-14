module GoatAST where

type Ident = String

data PType
    = BoolType
    | IntType
    | FloatType
    deriving (Show, Eq)

data Variable
    = Variable
    { varId                :: Ident
    , varSIndicator        :: SIndicator
    } deriving (Show, Eq)


data Expr
    = ExprVar
        { exprVar          :: Variable }
    | BoolConst
        { boolConstVal     :: Bool }
    | IntConst
        { intConstVal      :: Int }
    | FloatConst
        { floatConstVal    :: Float }
    | StrConst
        { strConstVal      :: String }
    | Add
        { addLeftExpr      :: Expr
        , addRightExpr     :: Expr
        }
    | Mul
        { mulLeftExpr      :: Expr
        , mulRightExpr     :: Expr
        }
    | Sub
        { subLeftExpr      :: Expr
        , subRightExpr     :: Expr
        }
    | Div
        { divLeftExpr      :: Expr
        , divRightExpr     :: Expr
        }
    | Or
        { orLeftExpr       :: Expr
        , orRightExpr      :: Expr
        }
    | And
        { andLeftExpr      :: Expr
        , andRightExpr     :: Expr
        }
    | Eq
        { eqLeftExpr       :: Expr
        , eqRightExpr      :: Expr
        }
    | NotEq
        { notEqLeftExpr    :: Expr
        , notEqRightExpr   :: Expr
        }
    | Les
        { lesLeftExpr      :: Expr
        , lesRightExpr     :: Expr
        }
    | LesEq
        { lesEqLeftExpr    :: Expr
        , lesEqRightExpr   :: Expr
        }
    | Grt
        { grtLeftExpr      :: Expr
        , grtRightExpr     :: Expr
        }
    | GrtEq
        { grtEqLeftExpr    :: Expr
        , grtEqRightExpr   :: Expr
        }
    | UnaryMinus
        { unaryMinusExpr   :: Expr }
    | UnaryNot
        { unaryNotExpr     :: Expr }
    deriving (Show, Eq)

data VariableDeclaration = VariableDeclaration { declarationType     :: PType
                                               , declarationVariable :: Variable
                                               } deriving (Show, Eq)

data Stmt
    = Assign
        { assignVal        :: Variable
        , assignExpr       :: Expr
        }
    | Read
        { readVal          :: Variable }
    | Write
        { writeExpr        :: Expr }
    | Call
        { callIdent        :: Ident
        , callExprs        :: [Expr]
        }
    | If
        { ifExpr           :: Expr
        , ifStmts          :: [Stmt]
        }
    | IfElse
        { ifElseExpr       :: Expr
        , ifElseStmts1     :: [Stmt]
        , ifElseStmts2     :: [Stmt]
        }
    | While
        { whileExpr        :: Expr
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

data SIndicator
    = Array
      { arrayExpr          :: Expr }
    | Matrix
      { matrixMExpr        :: Expr
      , matrixNExpr        :: Expr
      }
    | NoIndicator
    deriving (Show, Eq)

data Parameter
    = Parameter
    { passingIndicator     :: PIndicator
    , passingType          :: PType
    , passingIdent         :: Ident
    } deriving (Show, Eq)

data Header
    = Header
    { headerIdent          :: Ident
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
