module GoatAST where



type Ident = String

data PType
  = BoolType | IntType | FloatType
    deriving (Show, Eq)

data Lvalue
  = LId Ident
    deriving (Show, Eq)

data Expr
  = Id Ident
  | BoolConst Bool
  | IntConst Int
  | FloatConst Float
  | StrConst String
  | Expr Expr
  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Div Expr Expr
  | Or Expr Expr
  | And Expr Expr
  | Eq Expr Expr
  | NotEq Expr Expr
  | Les Expr Expr
  | LesEq Expr Expr
  | Grt Expr Expr
  | GrtEq Expr Expr
  | UnaryMinus Expr
  | UnaryNot Expr
    deriving (Show, Eq)

data VDecl = VDecl
    { vdelType       :: PType
    , vdelIdent      :: Ident
    , vdelSIndicator :: ()
    } deriving (Show, Eq)

data Stmt
  = Assign Lvalue Expr
  | Read Lvalue
  | Write Expr
  | Call Lvalue ()
  | If Expr [Stmt]
  | IfElse Expr [Stmt] [Stmt]
  | While Expr [Stmt]
    deriving (Show, Eq)

data Body = Body
    { bodyVarDeclarations :: [VDecl]
    , bodyStatements      :: [Stmt]
    } deriving (Show, Eq)

data Pindicator
  = VarType | RefType
      deriving (Show, Eq)

data Sindicator
  = Array Expr
  | Matrix (Expr,Expr)
      deriving (Show, Eq)

data Parameter = Parameter
    { passingIndicator :: Pindicator
    , passingType      :: PType
    , passingIdent     :: Ident
    } deriving (Show, Eq)

data Header = Header
    { headerIdent :: Ident
    , parameters :: [Parameter]
    } deriving (Show, Eq)

data Procedure = Procedure
    { header :: Header
    , body  :: Body
    } deriving (Show, Eq)

data GoatProgram = GoatProgram
    { procedures :: [Procedure]
    } deriving (Show, Eq)
