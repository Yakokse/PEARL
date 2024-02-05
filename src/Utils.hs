module Utils where

import AST
import Values
import Data.List (union, nub)

label :: Block a b -> a
label = fst . name

nonInput :: VariableDecl -> [Name]
nonInput decl = filter (`notElem` input decl) $ getVarsDecl decl

staticNonOutput :: VariableDecl' -> [Name]
staticNonOutput decl =
  let allVars = getVarsDecl' decl
      onlyBTStatic = filter (\(_,l) -> l == BTStatic) allVars
      nonOutput = filter (`notElem` output' decl) onlyBTStatic
  in map fst nonOutput

mapLabel :: (a -> b) -> [Block a c] -> [Block b c]
mapLabel f = mapProgram (\l _ -> f l) id

mapStore :: (b -> c) -> [Block a b] -> [Block a c]
mapStore = mapProgram const

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
    appFrom (Entry s) = Entry (g s)
    appFrom (From (l, s)) = From (f l s, g s)
    appFrom (Fi e (l1, s1) (l2, s2)) = Fi e (f l1 s1, g s1) (f l2 s2, g s2)
    appJump (Exit s) = Exit (g s)
    appJump (Goto (l, s)) = Goto (f l s, g s)
    appJump (If e (l1, s1) (l2, s2)) = If e (f l1 s1, g s1) (f l2 s2, g s2)

mapBoth :: ((a, b) -> (c, b)) -> [Block a b] -> [Block c b]
mapBoth f = map (mapBlock f)

mapBlock :: ((a, b) -> (c, b)) -> Block a b -> Block c b
mapBlock f b = Block
  { name = f $ name b
  , from = mapFrom f $ from b
  , body = body b
  , jump = mapJump f $ jump b
  }

mapFrom :: ((a, b) -> (c, b)) -> ComeFrom a b -> ComeFrom c b
mapFrom _ (Entry s) = Entry s
mapFrom f (From l) = From (f l)
mapFrom f (Fi e l1 l2) = Fi e (f l1) (f l2)

mapJump :: ((a, b) -> (c, b)) -> Jump a b -> Jump c b
mapJump f (Exit s) = Exit s
mapJump f (Goto l) = Goto (f l)
mapJump f (If e l1 l2) = If e (f l1) (f l2)

getVarsProg :: Program a b -> [Name]
getVarsProg (decl, _) = getVarsDecl decl

getVarsDecl :: VariableDecl -> [Name]
getVarsDecl decl = input decl `union` output decl `union` temp decl

getVarsDecl' :: VariableDecl' -> [(Name, Level)]
getVarsDecl' decl = input' decl `union` output' decl `union` temp' decl

getVarsPat :: Pattern -> [Name]
getVarsPat (QConst _) = []
getVarsPat (QVar n) = [n]
getVarsPat (QPair q1 q2) = getVarsPat q1 `union` getVarsPat q2

getVarsExp :: Expr -> [Name]
getVarsExp (Const _) = []
getVarsExp (Var n) = [n]
getVarsExp (Op _ e1 e2) = getVarsExp e1 `union` getVarsExp e2
getVarsExp (UOp _ e) = getVarsExp e

getEntryBlock :: [Block a b] -> EM (Block a b)
getEntryBlock p = 
  case filter isEntry p of
    [] -> Left "No entry point found"
    [b] -> Right b
    _ -> Left "Multiple entry points found"

getEntry :: [Block a b] -> EM (a,b)
getEntry p = name <$> getEntryBlock p

getEntryLabel :: [Block a b] -> a
getEntryLabel = label . head . filter isEntry

getExitBlock :: [Block a b] -> EM (Block a b)
getExitBlock p = 
  case filter isExit p of
    [] -> Left "No entry point found"
    [b] -> Right b
    _ -> Left "Multiple entry points found"

getEntry' :: [Block' a] -> EM a
getEntry' p = 
  case filter f p of
    [] -> Left "No entry point found"
    [b] -> Right (name' b)
    _ -> Left "Multiple entry points found"
  where 
    f b = case from' b of Entry' -> True; _ -> False

getBlock :: (Eq a, Eq b) => [Block a b] -> (a, b) -> Maybe (Block a b)
getBlock p l = 
  case filter (\b -> name b == l) p of
    [b] -> return b
    _   -> Nothing

getBlockUnsafe :: (Eq a, Eq b) => [Block a b] -> (a, b) -> Block a b
getBlockUnsafe p l = head $ filter (\b -> name b == l) p

getBlock' :: Eq a => [Block' a] -> a -> Maybe (Block' a)
getBlock' p l = 
  case filter (\b -> name' b == l) p of
    [b] -> return b
    _   -> Nothing

getBlockErr :: (Eq a, Eq b, Show a, Show b) => 
               [Block a b] -> (a, b) -> EM (Block a b)
getBlockErr p l = 
  case filter (\b -> name b == l) p of
    [b] -> return b
    []  -> Left $ "Block not found: " ++ show l
    _   -> Left $ "Multiple blocks found named: " ++ show l 

getBlockErr' :: (Eq a, Show a) => [Block' a] -> a -> EM (Block' a)
getBlockErr' p l = 
  case filter (\b -> name' b == l) p of
    [b] -> return b
    []  -> Left $ "Block not found: " ++ show l
    _   -> Left $ "Multiple blocks found named: " ++ show l 

isEntry :: Block a b -> Bool
isEntry b = case from b of Entry _ -> True; _ -> False

isExit :: Block a b -> Bool
isExit b = case jump b of Exit _ -> True; _ -> False

exitCount :: [Block a b] -> Int
exitCount = length . filter isExit

isExit' :: Block' a -> Bool
isExit' b = case jump' b of Exit' -> True; _ -> False

fromLabels :: Block a b -> [(a, b)]
fromLabels Block{from = Entry _} = []
fromLabels Block{from = From l} = [l]
fromLabels Block{from = Fi _ l1 l2} = [l1, l2]

jumpLabels :: Block a b -> [(a, b)]
jumpLabels Block{jump = Exit _} = []
jumpLabels Block{jump = Goto l} = [l]
jumpLabels Block{jump = If _ l1 l2} = [l1, l2]

nameIn :: (Eq a, Eq b) => (a,b) -> [Block a b] -> Bool
nameIn l = any (\b -> name b == l)

labels :: Eq a => [Block a b] -> [a]
labels = nub . map (fst . name)
