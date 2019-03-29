module GoatAST where



  type Ident = String

  data PType
    = BoolType | IntType | FloatType
      deriving (Show, Eq)

  data Lvalue
    = LId Ident
      deriving (Show, Eq)

  data Binop
    = Op_add Expr Expr
    | Op_mul Expr Expr
    | Op_min Expr Expr
    | Op_div Expr Expr
    | Op_or Expr Expr
    | Op_and Expr Expr
    | Op_eq Expr Expr
    | Op_neq Expr Expr
    | Op_les Expr Expr
    | Op_leseq Expr Expr
    | Op_grt Expr Expr
    | Op_grteq Expr Expr
      deriving (Show, Eq)

  data Unaryop
    = Op_not Expr
    | Op_neg Expr
      deriving (Show, Eq)

  data Expr
    = Id Ident
    | BoolConst Bool
    | IntConst Int
    | FloatConst Float
    | StrConst String
    | Expr Expr
    | Binop Binop Binop
    | Unaryop Unaryop
      deriving (Show, Eq)

  data VDecl
    = VDecl PType Ident
      deriving (Show, Eq)

  data Stmt
    = Assign Lvalue Expr
    | Read Lvalue
    | Write Expr
    | Call Ident [Expr]
    | If Expr [Stmt]
    | IfElse Expr [Stmt] [Stmt]
    | While Expr [Stmt]
      deriving (Show, Eq)

-- data Body
--   = Body VDecl Stmt
--       deriving (Show, Eq)

data Pindicator
  = VarType | RefType
      deriving (Show, Eq)

data Sindicator
  = Array | Matrix
      deriving (Show, Eq)

-- data Parameter
--   = Parameter Pindicator PType Ident
--       deriving (Show, Eq)
--
-- data Header
--   = Header Ident Parameter
--       deriving (Show, Eq)
--
-- data Procedure
--   = Procedure Header Body
--       deriving (Show, Eq)

data GoatProgram
  = Program Header Body
      deriving (Show, Eq)
