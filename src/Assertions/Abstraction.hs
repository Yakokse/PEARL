module Assertions.Abstraction where

import RL.AST
import RL.Values
import Control.Monad (guard)


-- None field vs Maybe
-- Just Pair (Nothing, Just AAtom) vs Nothing?
data AValue = Any | ANil | ANonNil | AAtom | APair AValue AValue
  deriving (Eq)

instance Show AValue where
  show Any           = "⊤"
  show ANil          = "Nil"
  show ANonNil       = "Non-Nil"
  show AAtom         = "Atom"
  show (APair v1 v2) = "(" ++ show v1 ++ "," ++ show v2 ++ ")"

aShow :: Maybe AValue -> String
aShow Nothing = "⊥"
aShow v       = show v

-- least upper bound
alub :: AValue -> AValue -> AValue
-- Any  `alub` _    = Any
-- _    `alub` Any  = Any
-- ANonNil `alub` ANil = Any
-- ANil `alub` ANonNil = Any
-- ANonNil `alub` _ = ANonNil
-- _ `alub` ANonNil = ANonNil
-- (APair v1 v2) `alub` (APair v1' v2') =
--   APair (v1 `alub` v1') (v2 `alub` v2')
a `alub` b =
  case (a, b) of
    (Any, _) -> Any
    (_, Any) -> Any
    -- Nils with non-any
    (ANil, ANil) -> ANil
    (ANil, _) -> Any
    (_, ANil) -> Any
    -- Non-Nil with non-any/nil
    (ANonNil, _) -> ANonNil
    (_, ANonNil) -> ANonNil
    -- Atom with non-any/nil/non-nil
    (AAtom, AAtom) -> AAtom
    (AAtom, _) -> ANonNil
    (_, AAtom) -> ANonNil
    -- Pair with nothing else
    (APair v1 v2, APair v1' v2') ->
      APair (v1 `alub` v1') (v2 `alub` v2')

-- greatest lower bound
aglb :: AValue -> AValue -> Maybe AValue
-- Any  `aglb` v    = return v
-- v    `aglb` Any  = return v
-- (APair v1 v2) `aglb` (APair v1' v2') =
--   do v1'' <- v1 `aglb` v1'
--      v2'' <- v2 `aglb` v2'
--      return $ APair v1'' v2''
-- v1   `aglb` v2   =
--   if v1 == v2 then return v1 else Nothing

a `aglb` b =
  case (a, b) of
    (Any, _) -> return b
    (_, Any) -> return a
    -- Nils with non-any
    (ANil, ANil) -> return ANil
    (ANil, _) -> Nothing
    (_, ANil) -> Nothing
    -- Non-Nil with non-any/nil
    (ANonNil, _) -> return b
    (_, ANonNil) -> return a
    -- Atom with non-any/nil/non-nil
    (AAtom, AAtom) -> return AAtom
    (AAtom, _) -> Nothing
    (_, AAtom) -> Nothing
    -- Pair with nothing else
    (APair v1 v2, APair v1' v2') ->
      APair <$> (v1 `aglb` v1') <*> (v2 `aglb` v2')

-- abstracted binary operators
aBinOp :: BinOp -> AValue -> AValue -> Maybe AValue
aBinOp (ROp op) v1 v2 = aRevOp op v1 v2
aBinOp Mul v1 v2      = numericOp v1 v2
aBinOp Div v1 v2      = numericOp v1 v2
aBinOp Less v1 v2     = numericCmp v1 v2
aBinOp Greater v1 v2  = numericCmp v1 v2
aBinOp Equal ANil ANil = return AAtom -- Trivial edge case
aBinOp Equal v1 v2 = -- If types match then true/nil else always nil
  if canEqual v1 v2 then return Any else return ANil
aBinOp Cons v1 v2 = return $ APair v1 v2 -- Exact information preserval
aBinOp And v _  | canNil v = return v -- nil -> nil, any -> any
aBinOp And _ v  = return v -- LHS always true, short-circuit to RHS
aBinOp Or ANil v = return v -- LHS false, short-circuit to RHS
aBinOp Or v   _  = return v -- any -> any, true val -> true val

-- abstracted reversible operators
aRevOp :: RevOp -> AValue -> AValue -> Maybe AValue
aRevOp Xor ANil v = return v  -- Neutral element for xor
aRevOp Xor v ANil = return v
aRevOp Xor Any _  = return Any -- Can either be nil or exact match
aRevOp Xor _  Any = return Any
aRevOp Xor v1 v2  = -- Non-nil vals must match
  do assert $ v1 `canEqual` v2
     return ANil -- If they are matchable non-nil vals, result must be nil
aRevOp Add v1 v2 = numericOp v1 v2
aRevOp Sub v1 v2 = numericOp v1 v2

-- All numeric comparisons take numbers and return true or nil
numericCmp :: AValue -> AValue -> Maybe AValue
numericCmp v1 v2 =
  do assert $ canNum v1 && canNum v2
     return Any

-- All numeric operations take numbers and return a number
numericOp :: AValue -> AValue -> Maybe AValue
numericOp v1 v2 =
  do assert $ canNum v1 && canNum v2
     return AAtom

-- Are there values of the two types that can be equal
canEqual :: AValue -> AValue -> Bool
-- canEqual Any _  = True -- One type is the universe and other is not empty
-- canEqual _ Any  = True
-- canEqual (APair v1 v2) (APair v1' v2') = -- Pairwise logic
--   canEqual v1 v1' && canEqual v2 v2'
-- canEqual v1 v2 = v1 == v2 -- v1 and v2 must be nil or an atom
a `canEqual` b =
  case (a, b) of
    (Any, _) -> True
    (_, Any) -> True

    (ANil, ANil) -> True
    (ANil, _) -> False
    (_, ANil) -> False

    (ANonNil, _) -> True
    (_, ANonNil) -> True

    (AAtom, AAtom) -> True
    (AAtom, _) -> False
    (_, AAtom) -> False

    (APair v1 v2, APair v1' v2') ->
      v1 `canEqual` v1' && v2 `canEqual` v2'

canNil :: AValue -> Bool
canNil ANil        = True
canNil Any         = True
canNil ANonNil     = False
canNil AAtom       = False
canNil (APair _ _) = False

canNum :: AValue -> Bool
canNum Any = True
canNum AAtom = True
canNum ANonNil = True
canNum ANil = False
canNum (APair _ _) = False

-- abstracted unary operator
aUnOp :: UnOp -> AValue -> Maybe AValue
aUnOp _ Any          = return Any   -- Hd/tl can be anything, not as well
aUnOp Hd (APair v _) = return v     -- Trivial
aUnOp Hd _           = Nothing  -- Value is never a pair
aUnOp Tl (APair _ v) = return v     -- Trivial
aUnOp Tl _           = Nothing  -- Value is never a pair
aUnOp Not ANil       = return AAtom -- Trivial true
aUnOp Not _          = return ANil  -- Value is never false

aValue :: Value -> AValue
aValue Nil = ANil
aValue (Pair v1 v2) = APair (aValue v1) (aValue v2)
aValue (Atom _) = AAtom
aValue (Num _) = AAtom

assert :: Bool -> Maybe ()
assert = guard
