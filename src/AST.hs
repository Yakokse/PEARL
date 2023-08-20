module AST where

type Name = String
type IntType = Word

type ErrMsg = String 
type EM = Either ErrMsg 

type Program label = [Block label]

data Block label = Block 
    { name :: label
    , from :: IfFrom label
    , body :: [Statement]
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

data Statement = 
      Update Place RevOp Expr
    | Push Name Name
    | Pop Name Name
    | Skip
    deriving (Eq, Show, Read)

data Place =
      Var Name
    | Arr Name Expr
    deriving (Eq, Show, Read)

data Expr =
      Const IntType
    | Place Place
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