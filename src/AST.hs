module AST where

import Values 

type Program label = [Block label]

data Block label = Block 
  { name :: label
  , from :: IfFrom label
  , body :: [Step]
  , jump :: Jump label
  }
  deriving (Eq, Show, Read)


-- TODO: Rename to Landing?
data IfFrom label = 
    From label 
  | FromCond Expr label label 
  | Entry
  deriving (Eq, Show, Read)

data Jump label = 
    Goto label 
  | If Expr label label 
  | Exit
  deriving (Eq, Show, Read)

data Step = 
    UpdateV Name RevOp Expr
  | Match Pattern Pattern
  | Assert Expr
  | Skip
  deriving (Eq, Show, Read)

data Expr =
    EConst Constant
  | EVar Name
  | EOp BinOp Expr Expr
  | EHd Expr
  | ETl Expr
  | EPair Expr Expr
  deriving (Eq, Show, Read)

data Pattern =
    PConst Constant
  | PVar Name
  | PPair Pattern Pattern
  deriving (Eq, Show, Read)

data Constant =
    Atom String
  | Num  IntType
  | Pair Constant Constant
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
  deriving (Eq, Show, Read)

data RevOp =
    Add
  | Sub
  | Xor
  deriving (Eq, Show, Read)

type Program' label = [Block' label]

data Level = Res | Elim
  deriving (Eq, Show, Read)

data Block' label = Block' 
  { name' :: label
  , from' :: IfFrom' label
  , body' :: [Step']
  , jump' :: Jump' label
  }
  deriving (Eq, Show, Read)

data IfFrom' label = 
    From' Level label
  | FromCond' Level Expr' label label 
  | Entry' Level
  deriving (Eq, Show, Read)

data Jump' label = 
    Goto' Level label 
  | If' Level Expr' label label 
  | Exit' Level
  deriving (Eq, Show, Read)

data Step' = 
    UpdateV' Level Name RevOp Expr'
  | UpdateA' Level Name Expr' RevOp Expr'
  | Push' Level Name Name
  | Pop' Level Name Name
  | Assert' Level Expr'
  | Skip' Level
  deriving (Eq, Show, Read)

data Expr' =
    Const' Level IntType
  | Var' Level Name
  | Arr' Level Name Expr'
  | Op' Level BinOp Expr' Expr'
  | Top' Level Name
  | Empty' Level Name
  | Lift Expr'
  deriving (Eq, Show, Read)