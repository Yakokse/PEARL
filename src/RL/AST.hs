module AST where

import Values

type Program label store = (VariableDecl, [Block label store])

data VariableDecl = VariableDecl
  { input  :: [Name]
  , output :: [Name]
  , temp   :: [Name]
  } deriving (Eq, Show, Read)

data Block label store = Block
  { name :: (label, store)
  , from :: ComeFrom label store
  , body :: [Step]
  , jump :: Jump label store
  }
  deriving (Eq, Show, Read)

data ComeFrom label store =
    From (label, store)
  | Fi Expr (label, store) (label, store)
  | Entry store
  deriving (Eq, Show, Read)

data Jump label store =
    Goto (label, store)
  | If Expr (label, store) (label, store)
  | Exit store
  deriving (Eq, Show, Read)

data Step =
    Update Name RevOp Expr
  | Replacement Pattern Pattern
  | Assert Expr
  | Skip
  deriving (Eq, Show, Read)

data Expr =
    Const Value
  | Var Name
  | Op BinOp Expr Expr
  | UOp UnOp Expr
  deriving (Eq, Show, Read)

data Pattern =
    QConst Value
  | QVar Name
  | QPair Pattern Pattern
  deriving (Eq, Show, Read)

data BinOp =
    ROp RevOp
  | Mul
  | Div
  | And
  | Or
  | Less
  | Greater
  | Equal
  | Cons
  deriving (Eq, Show, Read)

data RevOp =
    Add
  | Sub
  | Xor
  deriving (Eq, Show, Read)

data UnOp =
    Hd
  | Tl
  | Not
  deriving (Eq, Show, Read)

type NormProgram label = (VariableDecl, [NormBlock label])
data NormBlock label = NormBlock
  { nname :: label
  , nfrom :: ComeFrom label ()
  , nstep :: Step
  , njump :: Jump label ()
  }
  deriving (Eq, Show, Read)
