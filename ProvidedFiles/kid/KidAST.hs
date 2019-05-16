module KidAST where

-----------------------------------
-- Specification of an AST for Goat 
-----------------------------------

type Ident = String
 
data BaseType 
  = BoolType | IntType 
    deriving (Show, Eq)

data Lvalue 
  = LId Ident
    deriving (Show, Eq)

data Binop 
  = Op_add | Op_mul 
    deriving (Show, Eq)

data Expr
  = BoolConst Bool
  | IntConst Int
  | StrConst String
  | Id Ident
  | Add Expr Expr
  | Mul Expr Expr
  | UnaryMinus Expr
    deriving (Show, Eq)

data Decl 
  = Decl Ident BaseType
    deriving (Show, Eq)

data Statement 
  = Assign Lvalue Expr
  | Read Lvalue
  | Write Expr
  | Call Ident [Expr]
  | If Expr [Statement]
  | IfElse Expr [Statement] [Statement]
  | While Expr [Statement]
    deriving (Show, Eq)

data KidProgram
  = Program [Decl] [Statement]
    deriving (Show, Eq)

