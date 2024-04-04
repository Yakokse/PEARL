module Assertions.Abstraction where

import RL.AST
import RL.Values
import Control.Monad (guard)

-- Partial lattice, bottom is prepresented by Nothing in Maybe AValue
-- So APair cannot contain bottom
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

lte :: Maybe AValue -> Maybe AValue -> Bool
Nothing `lte` _ = True
_ `lte` Nothing = False
(Just a) `lte` (Just b) = a `lteStrict` b

lteStrict :: AValue -> AValue -> Bool
a `lteStrict` b =
  case (a, b) of
    (_, Any) -> True
    (Any, _) -> False

    (ANil, ANil) -> True
    (ANil, _) -> False
    (_, ANil) -> False

    (ANonNil, ANonNil) -> True
    (AAtom, ANonNil) -> True
    (APair _ _, ANonNil) -> True
    (ANonNil, _) -> False

    (AAtom, AAtom) -> True
    (AAtom, _) -> False
    (_, AAtom) -> False

    (APair v1 v2, APair v1' v2') ->
      (v1 `lteStrict` v1') && (v2 `lteStrict` v2')

-- abstracted binary operators
aBinOp :: BinOp -> AValue -> AValue -> Maybe AValue
aBinOp (ROp op) v1 v2 = aRevOp op v1 v2
aBinOp Mul v1 v2      = numericOp v1 v2
aBinOp Div v1 v2      = numericOp v1 v2
aBinOp Less v1 v2     = numericCmp v1 v2
aBinOp Greater v1 v2  = numericCmp v1 v2
aBinOp Equal ANil ANil = return AAtom -- Trivial edge case
aBinOp Equal v1 v2 = -- If types match then true/nil else always nil
  if v1 `aglb` v2 /= Nothing then return Any else return ANil
aBinOp Cons v1 v2 = return $ APair v1 v2 -- Exact information preserval
aBinOp And v1 v2 =
  return $ case (v1, v2) of
            (ANil, _) -> ANil -- always false
            (_, ANil) -> ANil -- always false
            (Any, _) -> Any -- Not known if lhs is true
            _ -> v2 -- v1 must be <= nonNil, so true
aBinOp Or ANil v = return v -- LHS false, short-circuit to RHS
aBinOp Or v   _  = return v -- any -> any, true val -> true val

-- abstracted reversible operators
aRevOp :: RevOp -> AValue -> AValue -> Maybe AValue
aRevOp Xor ANil v = return v  -- Neutral element for xor
aRevOp Xor v ANil = return v
aRevOp Xor Any _  = return Any -- Can either be nil or exact match
aRevOp Xor _  Any = return Any
aRevOp Xor v1 v2  = -- Non-nil vals must match
  do assert $ v1 `aglb` v2 /= Nothing
     return ANil -- If they are matchable non-nil vals, result must be nil
aRevOp Add v1 v2 = numericOp v1 v2
aRevOp Sub v1 v2 = numericOp v1 v2

-- All numeric comparisons take numbers and return true or nil
numericCmp :: AValue -> AValue -> Maybe AValue
numericCmp v1 v2 =
  do assert $ AAtom `lteStrict` v1
     assert $ AAtom `lteStrict` v2
     return Any

-- All numeric operations take numbers and return a number
numericOp :: AValue -> AValue -> Maybe AValue
numericOp v1 v2 =
  do assert $ AAtom `lteStrict` v1
     assert $ AAtom `lteStrict` v2
     return AAtom

-- abstracted unary operator
aUnOp :: UnOp -> AValue -> Maybe AValue
aUnOp Hd v =
  case v of
    (APair v1 _) -> return v1
    _ | APair Any Any `lteStrict` v -> return Any
    _            -> Nothing

aUnOp Tl v =
  case v of
    (APair _ v2) -> return v2
    _ | APair Any Any `lteStrict` v -> return Any
    _            -> Nothing

aUnOp Not v =
  case v of
    Any  -> return Any
    ANil -> return AAtom
    _    -> return ANil

-- abstraction
aValue :: Value -> AValue
aValue Nil = ANil
aValue (Pair v1 v2) = APair (aValue v1) (aValue v2)
aValue (Atom _) = AAtom
aValue (Num _) = AAtom

-- Get bottom when false
assert :: Bool -> Maybe ()
assert = guard
