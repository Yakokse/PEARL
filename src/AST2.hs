module AST2 where

import AST

type Program' label = [Block' label]

data Block' label = Block' 
    { name' :: label
    , from' :: IfFrom' label
    , body' :: [Statement']
    , goto' :: IfGoto' label
    }
    deriving (Eq, Show, Read)

data IfFrom' label = 
      From' label
    | From2 label 
    | FromCond' Expr' label label 
    | FromCond2 Expr' label label 
    | Entry'
    | Entry2
    deriving (Eq, Show, Read)

data IfGoto' label = 
      Goto' label 
    | Goto2 label
    | GotoCond' Expr' label label 
    | GotoCond2 Expr' label label 
    | Exit'
    | Exit2
    deriving (Eq, Show, Read)

data Statement' = 
      UpdateV' Name RevOp Expr'
    | UpdateV2 Name RevOp Expr'
    | UpdateA' Name Expr' RevOp Expr'
    | UpdateA2 Name Expr' RevOp Expr'
    | Push' Name Name
    | Push2 Name Name
    | Pop' Name Name
    | Pop2 Name Name
    | Skip'
    | Skip2
    deriving (Eq, Show, Read)

data Expr' =
      Const' IntType
    | Const2 IntType
    | Var' Name
    | Var2 Name
    | Arr' Name Expr'
    | Arr2 Name Expr'
    | Op' BinOp Expr' Expr'
    | Op2 BinOp Expr' Expr'
    | Top' Name
    | Top2 Name
    | Empty' Name
    | Empty2 Name
    | Lift Expr'
    deriving (Eq, Show, Read)