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
getVarsStep (Update n _ e) = [n] `union` getVarsExp e
getVarsStep (Replacement q1 q2) = getVarsPat q1 `union` getVarsPat q2
getVarsStep (Assert e) = getVarsExp e
getVarsStep Skip = []

getVarsPat :: Pattern -> [Name]
getVarsPat (QConst _) = []
getVarsPat (QVar n) = [n]
getVarsPat (QPair q1 q2) = getVarsPat q1 `union` getVarsPat q2

getVarsExp :: Expr -> [Name]
getVarsExp (Const _) = []
getVarsExp (Var n) = [n]
getVarsExp (Op _ e1 e2) = getVarsExp e1 `union` getVarsExp e2
getVarsExp (UOp _ e) = getVarsExp e


getEntry :: Program a -> EM a
getEntry p = 
  case filter f p of
    [] -> Left "No entry point found"
    [b] -> Right (name b)
    _ -> Left "Multiple entry points found"
  where 
    f b = case from b of Entry -> True; _ -> False

getExitBlock :: Program a -> EM (Block a)
getExitBlock p = 
  case filter f p of
    [] -> Left "No entry point found"
    [b] -> Right b
    _ -> Left "Multiple entry points found"
  where 
    f b = case jump b of Exit -> True; _ -> False

getEntryBlock :: Program a -> EM (Block a)
getEntryBlock p = 
  case filter f p of
    [] -> Left "No entry point found"
    [b] -> Right b
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
    f b = case from' b of Entry' -> True; _ -> False

getBlock :: Eq a => Program a -> a -> Maybe (Block a)
getBlock p l = 
  case filter (\b -> name b == l) p of
    [b] -> return b
    _   -> Nothing

getBlock' :: Eq a => Program' a -> a -> Maybe (Block' a)
getBlock' p l = 
  case filter (\b -> name' b == l) p of
    [b] -> return b
    _   -> Nothing

getBlockErr :: Eq a => (a -> String) -> Program a -> a -> EM (Block a)
getBlockErr format p l = 
  case filter (\b -> name b == l) p of
    [b] -> return b
    []  -> Left $ "Block not found: " ++ format l
    _   -> Left $ "Multiple blocks found named: " ++ format l 

getBlockErr' :: Eq a => (a -> String) -> Program' a -> a -> EM (Block' a)
getBlockErr' format p l = 
  case filter (\b -> name' b == l) p of
    [b] -> return b
    []  -> Left $ "Block not found: " ++ format l
    _   -> Left $ "Multiple blocks found named: " ++ format l 

isExit :: Block a -> Bool
isExit b = case jump b of Exit -> True; _ -> False

isExit' :: Block' a -> Bool
isExit' b = case jump' b of Exit' -> True; _ -> False

fromLabels :: Block a -> [a]
fromLabels Block{from = Entry} = []
fromLabels Block{from = From l} = [l]
fromLabels Block{from = FromCond _ l1 l2} = [l1, l2]

jumpLabels :: Block a -> [a]
jumpLabels Block{jump = Exit} = []
jumpLabels Block{jump = Goto l} = [l]
jumpLabels Block{jump = If _ l1 l2} = [l1, l2]

nameIn :: Eq a => a -> Program a -> Bool
nameIn l = any (\b -> name b == l)