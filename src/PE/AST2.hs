module PE.AST2 where

import RL.AST
import RL.Values

import PE.SpecValues
import PE.Preprocessing.Division

type Program' label = [Block' label]

data Explicated label = Regular label | Explicator label [Name]
  deriving (Eq, Show, Read)

data Block' label = Block'
  { name' :: label
  , initDiv :: Division
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
  | Replacement' Level Pattern' Pattern'
  | Assert' Level Expr'
  | Skip' Level
  | Generalize Name
  deriving (Eq, Show, Read)

data Pattern' =
    QConst' Level Value
  | QVar' Level Name
  | QPair' Level Pattern' Pattern'
  | Drop Name
  deriving (Eq, Show, Read)

data Expr' =
    Const' Level Value
  | Var' Level Name
  | Op' Level BinOp Expr' Expr'
  | UOp' Level UnOp Expr'
  | Lift Expr'
  deriving (Eq, Show, Read)
