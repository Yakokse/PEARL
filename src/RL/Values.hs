module RL.Values where

import qualified Data.Map.Strict as Map

type IntType = Word
type Name = String
type Label = String

data Value =
    Atom String
  | Num  IntType
  | Pair Value Value
  | Nil
  deriving (Eq, Show, Read, Ord)

type Store = Map.Map Name Value

trueV :: Value
trueV = Atom "true"

falseV :: Value
falseV = Nil

truthy :: Value -> Bool
truthy = (/= falseV)

boolify :: Bool -> Value
boolify b = if b then trueV else falseV

findErr ::  Store -> Name -> Value
findErr s n =
  case Map.lookup n s of
    Just v -> v
    _ -> error $ "static variable " ++ n ++ " not found during lookup"
