module Utils where

import AST
import AST2
import Values
import Data.List (union)

changeLabel :: (a -> b) -> Program a -> Program b
changeLabel t = map (block t)
    where block f b = Block
            { name = f $ name b
            , from = appFrom f $ from b
            , body = body b
            , goto = appGoto f $ goto b
            }
          appFrom _ Entry = Entry
          appFrom f (From l) = From (f l)
          appFrom f (FromCond e l1 l2) = FromCond e (f l1) (f l2)
          appGoto _ Exit = Exit
          appGoto f (Goto l) = Goto (f l)
          appGoto f (GotoCond e l1 l2) = GotoCond e (f l1) (f l2)

getVarsProg :: Program a -> [Name]
getVarsProg = foldl helper []
    where helper acc block  = acc `union` getVarsBlock block

getVarsBlock :: Block a -> [Name]
getVarsBlock b = bodyVars `union` fromVars `union` gotoVars
    where 
        bodyVars = foldl (\acc e -> acc `union` getVarsStep  e) [] $ body b
        fromVars = getVarsFrom . from $ b
        gotoVars = getVarsGoto . goto $ b

getVarsFrom :: IfFrom a -> [Name]
getVarsFrom (FromCond e _ _) = getVarsExp e
getVarsFrom _ = []

getVarsGoto :: IfGoto a -> [Name]
getVarsGoto (GotoCond e _ _) = getVarsExp e
getVarsGoto _ = []

getVarsStep  :: Step -> [Name]
getVarsStep  (UpdateV n _ e) = [n] `union` getVarsExp e
getVarsStep  (UpdateA n e1 _ e2) = [n] `union` getVarsExp e1 `union` getVarsExp e2
getVarsStep  (Push n1 n2) = [n1] `union` [n2]
getVarsStep  (Pop n1 n2) = [n1] `union` [n2]
getVarsStep  Skip = []

getVarsExp :: Expr -> [Name]
getVarsExp (Const _) = []
getVarsExp (Var n) = [n]
getVarsExp (Arr n e) = [n] `union` getVarsExp e
getVarsExp (Op _ e1 e2) = getVarsExp e1 `union` getVarsExp e2
getVarsExp (Top n) = [n]
getVarsExp (Empty n) = [n]

findEntry :: Program a -> EM a
findEntry p = 
    case filter f p of
        [] -> Left "No entry point found"
        [b] -> Right (name b)
        _ -> Left "Multiple entry points found"
    where 
        f b = case from b of Entry -> True; _ -> False

findEntry' :: Program' a -> EM a
findEntry' p = 
    case filter f p of
        [] -> Left "No entry point found"
        [b] -> Right (name' b)
        _ -> Left "Multiple entry points found"
    where 
        f b = case from' b of Entry' _ -> True; _ -> False