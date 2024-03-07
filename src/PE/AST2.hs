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

mapProg' :: (a -> b) -> Program' a -> Program' b
mapProg' f = map (mapBlock' f)

mapBlock' :: (a -> b) -> Block' a -> Block' b
mapBlock' f b = b
  { name' = f $ name' b
  , from' = mapFrom' f $ from' b
  , jump' = mapJump' f $ jump' b
  }

mapFrom' :: (a -> b) -> ComeFrom' a -> ComeFrom' b
mapFrom' _ Entry' = Entry'
mapFrom' f (From' l) = From' (f l)
mapFrom' f (Fi' l e l1 l2) = Fi' l e (f l1) (f l2)

mapJump' :: (a -> b) -> Jump' a -> Jump' b
mapJump' _ Exit' = Exit'
mapJump' f (Goto' l) = Goto' (f l)
mapJump' f (If' l e l1 l2) = If' l e (f l1) (f l2)

isExit' :: Block' a -> Bool
isExit' b = case jump' b of Exit' -> True; _ -> False

fromLabels' :: ComeFrom' a -> [a]
fromLabels' Entry' = []
fromLabels' (From' l) = [l]
fromLabels' (Fi' _ _ l1 l2) = [l1, l2]

jumpLabels' :: Jump' a -> [a]
jumpLabels' Exit' = []
jumpLabels' (Goto' l) = [l]
jumpLabels' (If' _ _ l1 l2) = [l1, l2]
