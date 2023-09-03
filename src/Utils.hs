module Utils where

import AST
import Values
import Data.List (union)

changeLabel :: (a -> b) -> Program a -> Program b
changeLabel t = map (block t)
  where 
    block f b = Block
      { name = f $ name b
      , from = appFrom f $ from b
      , body = body b
      , jump = appJump f $ jump b
      }
    appFrom _ Entry = Entry
    appFrom f (From l) = From (f l)
    appFrom f (FromCond e l1 l2) = FromCond e (f l1) (f l2)
    appJump _ Exit = Exit
    appJump f (Goto l) = Goto (f l)
    appJump f (If e l1 l2) = If e (f l1) (f l2)

getVarsProg :: Program a -> [Name]
getVarsProg = foldl helper []
    where helper acc block  = acc `union` getVarsBlock block

getVarsBlock :: Block a -> [Name]
getVarsBlock b = bodyVars `union` fromVars `union` gotoVars
  where 
    bodyVars = foldl (\acc e -> acc `union` getVarsStep  e) [] $ body b
    fromVars = getVarsFrom . from $ b
    gotoVars = getVarsJump . jump $ b

getVarsFrom :: IfFrom a -> [Name]
getVarsFrom (FromCond e _ _) = getVarsExp e
getVarsFrom _ = []

getVarsJump :: Jump a -> [Name]
getVarsJump (If e _ _) = getVarsExp e
getVarsJump _ = []

getVarsStep :: Step -> [Name]
getVarsStep (UpdateV n _ e) = [n] `union` getVarsExp e
getVarsStep (UpdateA n e1 _ e2) = [n] `union` getVarsExp e1 `union` getVarsExp e2
getVarsStep (Push n1 n2) = [n1] `union` [n2]
getVarsStep (Pop n1 n2) = [n1] `union` [n2]
getVarsStep Skip = []

getVarsExp :: Expr -> [Name]
getVarsExp (Const _) = []
getVarsExp (Var n) = [n]
getVarsExp (Arr n e) = [n] `union` getVarsExp e
getVarsExp (Op _ e1 e2) = getVarsExp e1 `union` getVarsExp e2
getVarsExp (Top n) = [n]
getVarsExp (Empty n) = [n]

getEntry :: Program a -> EM a
getEntry p = 
  case filter f p of
    [] -> Left "No entry point found"
    [b] -> Right (name b)
    _ -> Left "Multiple entry points found"
  where 
    f b = case from b of Entry -> True; _ -> False

getEntry' :: Program' a -> EM a
getEntry' p = 
  case filter f p of
    [] -> Left "No entry point found"
    [b] -> Right (name' b)
    _ -> Left "Multiple entry points found"
  where 
    f b = case from' b of Entry' _ -> True; _ -> False
    
getBlock :: (Eq a, Show a) => Program a -> a -> EM (Block a)
getBlock p l = 
  case filter (\b -> name b == l) p of
    [b] -> return b
    []  -> Left $ "Block not found: " ++ show l
    _   -> Left $ "Multiple blocks found named: " ++ show l 

getBlock' :: (Eq a, Show a) => Program' a -> a -> EM (Block' a)
getBlock' p l = 
  case filter (\b -> name' b == l) p of
    [b] -> return b
    []  -> Left $ "Block not found: " ++ show l
    _   -> Left $ "Multiple blocks found named: " ++ show l 

isExit :: Block a -> Bool
isExit b = case jump b of Exit -> True; _ -> False

isExit' :: Block' a -> Bool
isExit' b = case jump' b of Exit' _ -> True; _ -> False