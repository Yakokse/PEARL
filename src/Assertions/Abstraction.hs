module Assertions.Abstraction where

import AST

data AValue = Any | None | ANil | AAtom | APair AValue AValue
  deriving (Eq)

instance Show AValue where
  show Any           = "⊤"
  show None          = "⊥"
  show ANil          = "Nil"
  show AAtom         = "Atom"
  show (APair v1 v2) = "(" ++ show v1 ++ "," ++ show v2 ++ ")"

-- least upper bound
alub :: AValue -> AValue -> AValue
Any  `alub` _    = Any
_    `alub` Any  = Any
None `alub` v    = v
v    `alub` None = v
(APair v1 v2) `alub` (APair v1' v2') =
  APair (v1 `alub` v1') (v2 `alub` v2')
v1   `alub` v2   = -- reflexivity, otherwise flat hierarchy
  if v1 == v2 then v1 else Any

-- greatest lower bound
aglb :: AValue -> AValue -> AValue
Any  `aglb` v    = v
v    `aglb` Any  = v
None `aglb` _    = None
_    `aglb` None = None
(APair v1 v2) `aglb` (APair v1' v2') =
  APair (v1 `aglb` v1') (v2 `aglb` v2')
v1   `aglb` v2   = -- reflexivity, otherwise flat hierarchy
  if v1 == v2 then v1 else None

-- abstracted binary operators
aBinOp :: BinOp -> AValue -> AValue -> AValue
aBinOp _ None _ = None -- Propagate bots
aBinOp _ _ None = None
aBinOp (ROp op) v1 v2 = aRevOp op v1 v2
aBinOp Mul v1 v2      = numericOp v1 v2
aBinOp Div v1 v2      = numericOp v1 v2
aBinOp Less v1 v2     = numericCmp v1 v2
aBinOp Greater v1 v2  = numericCmp v1 v2
aBinOp Equal v1 v2 = -- If types match then true/nil else always nil
  if canEqual v1 v2 then Any else ANil
aBinOp Cons v1 v2 = APair v1 v2 -- Exact information preserval
aBinOp And ANil _ = ANil -- Will always be false
aBinOp And _ ANil = ANil
aBinOp And Any  _ = Any -- Either short-circuit or lhs
aBinOp And _    v = v -- LHS always true, short-circuit to RHS
aBinOp Or ANil v = v -- LHS false, short-circuit to RHS
aBinOp Or Any _  = Any -- Either short-circuit or lhs
aBinOp Or v   _  = v -- LHS always true, return it

-- abstracted reversible operators
aRevOp :: RevOp -> AValue -> AValue -> AValue
aRevOp _ None _   = None -- Propagate bots
aRevOp _ _ None   = None
aRevOp Xor ANil v = v  -- Neutral element for xor
aRevOp Xor v ANil = v
aRevOp Xor Any _  = Any -- Can either be nil or exact match
aRevOp Xor _  Any = Any
aRevOp Xor v1 v2  = -- Non-nil vals must match
  if v1 `canEqual` v2
    then ANil -- If they are matchable non-nil vals, result must be nil
    else None -- If they are not matchable, error must occur
aRevOp Add v1 v2 = numericOp v1 v2
aRevOp Sub v1 v2 = numericOp v1 v2

-- All numeric comparisons take numbers and return true or nil
numericCmp :: AValue -> AValue -> AValue
numericCmp v1 v2 = if numericInp v1 v2 then Any else None

-- All numeric operations take numbers and return a number
numericOp :: AValue -> AValue -> AValue
numericOp v1 v2 = if numericInp v1 v2 then AAtom else None

-- Verify that two inputs can be numbers
numericInp :: AValue -> AValue -> Bool
numericInp v1 v2 = canNum v1 && canNum v2
  where
    canNum Any = True
    canNum AAtom = True
    canNum _ = False

-- Are there values of the two types that can be equal
canEqual :: AValue -> AValue -> Bool
canEqual None _ = False -- No values in bot
canEqual _ None = False
canEqual Any _  = True -- One type is the universe and other is not empty
canEqual _ Any  = True
canEqual (APair v1 v2) (APair v1' v2') = -- Pairwise logic
  canEqual v1 v1' && canEqual v2 v2'
canEqual v1 v2 = v1 == v2 -- v1 and v2 must be nil or an atom

-- abstracted unary operator
aUnOp :: UnOp -> AValue -> AValue
aUnOp _ None         = None  -- Propagate bot
aUnOp _ Any          = Any   -- Hd/tl can be anything, not as well
aUnOp Hd (APair v _) = v     -- Trivial
aUnOp Hd _           = None  -- Value is never a pair
aUnOp Tl (APair _ v) = v     -- Trivial
aUnOp Tl _           = None  -- Value is never a pair
aUnOp Not ANil       = AAtom -- Trivial true
aUnOp Not _          = ANil  -- Value is never false
