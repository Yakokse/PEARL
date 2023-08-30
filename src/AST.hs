module AST where

import Values 

type Program label = [Block label]

data Block label = Block 
    { name :: label
    , from :: IfFrom label
    , body :: [Step]
    , goto :: IfGoto label
    }
    deriving (Eq, Show, Read)

data IfFrom label = 
      From label 
    | FromCond Expr label label 
    | Entry
    deriving (Eq, Show, Read)

data IfGoto label = 
      Goto label 
    | GotoCond Expr label label 
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
    deriving (Eq, Show, Read)

data RevOp =
      Add
    | Sub
    | Xor
    deriving (Eq, Show, Read)