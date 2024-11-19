module RL.AST where

import RL.Values

type Program label store = [Process label store]

--TODO: remove when implementing support for PE with processes
data VariableDecl = VariableDecl
  { input  :: [Name]
  , output :: [Name]
  , temp   :: [Name]
  } deriving (Eq, Show, Read)


data Process label store = Process 
  { pname :: ProcessName
  , pbody :: [Block label store]
  }
  deriving (Eq, Show, Read)

data Block label store = Block
  { name :: (label, store)
  , from :: ComeFrom label store
  , body :: [Step]
  , jump :: Jump label store
  }
  deriving (Eq, Show, Read)

data ComeFrom label store =
    From (label, store)
  | Fi Expr (label, store) (label, store)
  | Entry Pattern store
  deriving (Eq, Show, Read)

data Jump label store =
    Goto (label, store)
  | If Expr (label, store) (label, store)
  | Exit Pattern store
  deriving (Eq, Show, Read)

data Step =
    Update Name RevOp Expr
  | Replacement Pattern Pattern
  | Assert Expr
  | Skip
  deriving (Eq, Show, Read)

data Expr =
    Const Value
  | Var Name
  | Op BinOp Expr Expr
  | UOp UnOp Expr
  deriving (Eq, Show, Read)

data Pattern =
    QConst Value
  | QVar Name
  | QPair Pattern Pattern
  | QCall ProcessName Pattern
  | QUncall ProcessName Pattern
  deriving (Eq, Show, Read)

data BinOp =
    ROp RevOp
  | Mul
  | Div
  | And
  | Or
  | Less
  | Greater
  | Equal
  | Cons
  deriving (Eq, Show, Read)

data RevOp =
    Add
  | Sub
  | Xor
  deriving (Eq, Show, Read)

data UnOp =
    Hd
  | Tl
  | Not
  deriving (Eq, Show, Read)

type NormProgram label = [NormProcess label]
data NormProcess label = NormProcess 
  { npname :: ProcessName
  , npbody :: [NormBlock label]
  }
  deriving (Eq, Show, Read)

data NormBlock label = NormBlock
  { nname :: label
  , nfrom :: ComeFrom label ()
  , nstep :: Step
  , njump :: Jump label ()
  }
  deriving (Eq, Show, Read)

label :: Block a b -> a
label = fst . name

mapLabel :: (a -> b) -> [Block a c] -> [Block b c]
mapLabel f = mapProgram (\l _ -> f l) id

mapProgStore :: (b -> c) -> [Block a b] -> [Block a c]
mapProgStore = mapProgram const

mapCombine :: (a -> b -> c) -> [Block a b] -> [Block c ()]
mapCombine f = mapProgram f (const ())

mapProgram :: (a -> b -> c) -> (b -> d) ->
              [Block a b] -> [Block c d]
mapProgram f g = map changeBlock
  where
    changeBlock b = Block
      { name = appName $ name b
      , from = appFrom $ from b
      , body = body b
      , jump = appJump $ jump b
      }
    appName (l, s) = (f l s, g s)
    appFrom (Entry p s) = Entry p (g s)  --TODO: is correct?
    appFrom (From (l, s)) = From (f l s, g s)
    appFrom (Fi e (l1, s1) (l2, s2)) = Fi e (f l1 s1, g s1) (f l2 s2, g s2)
    appJump (Exit p s) = Exit p (g s) -- TODO: is correct?
    appJump (Goto (l, s)) = Goto (f l s, g s)
    appJump (If e (l1, s1) (l2, s2)) = If e (f l1 s1, g s1) (f l2 s2, g s2)

mapBoth :: ((a, b) -> (c, b)) -> [Block a b] -> [Block c b]
mapBoth f = map (mapBlock f)

mapBlock :: ((a, b) -> (c, b)) -> Block a b -> Block c b
mapBlock f b = b
  { name = f $ name b
  , from = mapFrom f $ from b
  , jump = mapJump f $ jump b
  }

mapFrom :: ((a, b) -> (c, b)) -> ComeFrom a b -> ComeFrom c b
mapFrom _ (Entry p s) = Entry p s --TODO: is correct?
mapFrom f (From l) = From (f l)
mapFrom f (Fi e l1 l2) = Fi e (f l1) (f l2)

mapJump :: ((a, b) -> (c, b)) -> Jump a b -> Jump c b
mapJump _ (Exit p s) = Exit p s
mapJump f (Goto l) = Goto (f l)
mapJump f (If e l1 l2) = If e (f l1) (f l2)

isEntry :: Block a b -> Bool
isEntry = isFromEntry . from

isExit :: Block a b -> Bool
isExit = isJumpExit . jump

isNEntry :: NormBlock a -> Bool
isNEntry = isFromEntry . nfrom

isNExit :: NormBlock a -> Bool
isNExit = isJumpExit . njump

isFromEntry :: ComeFrom a b -> Bool
isFromEntry j = case j of Entry _ _ -> True; _ -> False

isJumpExit :: Jump a b -> Bool
isJumpExit j = case j of Exit _ _-> True; _ -> False

fromLabels :: ComeFrom a b -> [(a, b)]
fromLabels (Entry _ _) = []
fromLabels (From l) = [l]
fromLabels (Fi _ l1 l2) = [l1, l2]

jumpLabels :: Jump a b -> [(a, b)]
jumpLabels (Exit _ _) = []
jumpLabels (Goto l) = [l]
jumpLabels (If _ l1 l2) = [l1, l2]
