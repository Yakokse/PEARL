module Utils where

import AST
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
        bodyVars = foldl (\acc e -> acc `union` getVarsStat e) [] $ body b
        fromVars = getVarsFrom . from $ b
        gotoVars = getVarsGoto . goto $ b

getVarsFrom :: IfFrom a -> [Name]
getVarsFrom (FromCond e _ _) = getVarsExp e
getVarsFrom _ = []

getVarsGoto :: IfGoto a -> [Name]
getVarsGoto (GotoCond e _ _) = getVarsExp e
getVarsGoto _ = []

getVarsStat :: Statement -> [Name]
getVarsStat (UpdateV n _ e) = [n] `union` getVarsExp e
getVarsStat (UpdateA n e1 _ e2) = [n] `union` getVarsExp e1 `union` getVarsExp e2
getVarsStat (Push n1 n2) = [n1] `union` [n2]
getVarsStat (Pop n1 n2) = [n1] `union` [n2]
getVarsStat Skip = []

getVarsExp :: Expr -> [Name]
getVarsExp (Const _) = []
getVarsExp (Var n) = [n]
getVarsExp (Arr n e) = [n] `union` getVarsExp e
getVarsExp (Op _ e1 e2) = getVarsExp e1 `union` getVarsExp e2
getVarsExp (Top n) = [n]
getVarsExp (Empty n) = [n]
