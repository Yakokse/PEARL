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
  | UpdateA Name Expr RevOp Expr
  | Push Name Name
  | Pop Name Name
  | Skip
  deriving (Eq, Show, Read)

data Expr =
    Const IntType
  | Var Name
  | Arr Name Expr
  | Op BinOp Expr Expr
  | Top Name
  | Empty Name
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