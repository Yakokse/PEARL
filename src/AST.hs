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
  | Index
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

type Program' label = (VariableDecl', [Block' label])

data VariableDecl' = VariableDecl' 
  { input'  :: [(Name, Level)]
  , output' :: [(Name, Level)]
  , temp'   :: [(Name, Level)] 
  }
  deriving (Eq, Show, Read)

data Block' label = Block' 
  { name' :: label
  , from' :: ComeFrom' label
  , body' :: [Step']
  , jump' :: Jump' label
  }
  deriving (Eq, Show, Read)

data ComeFrom' label = 
    From' label
  | Fi' Level Expr' label label 
  | Entry'
  deriving (Eq, Show, Read)

data Jump' label = 
    Goto' label 
  | If' Level Expr' label label 
  | Exit'
  deriving (Eq, Show, Read)

data Step' = 
    Update' Level Name RevOp Expr'
  | Replacement' Level Pattern Pattern
  | Assert' Level Expr'
  | Skip' Level
  deriving (Eq, Show, Read)

data Expr' =
    Const' Level Value
  | Var' Level Name
  | Op' Level BinOp Expr' Expr'
  | UOp' Level UnOp Expr'
  | Lift Expr'
  deriving (Eq, Show, Read)
  