module AST2 where

import AST
import Values 

type Program' label = [Block' label]

data Level = Res | Elim
  deriving (Eq, Show, Read)

data Block' label = Block' 
    { name' :: label
    , from' :: IfFrom' label
    , body' :: [Step']
    , goto' :: IfGoto' label
    }
    deriving (Eq, Show, Read)

data IfFrom' label = 
      From' Level label
    | FromCond' Level Expr' label label 
    | Entry' Level
    deriving (Eq, Show, Read)

data IfGoto' label = 
      Goto' Level label 
    | GotoCond' Level Expr' label label 
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