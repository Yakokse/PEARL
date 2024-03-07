module RL.Variables where

import RL.AST
import RL.Values

import Data.List (union)

nonInput :: VariableDecl -> [Name]
nonInput decl = filter (`notElem` input decl) $ allVars decl

nonOutput :: VariableDecl -> [Name]
nonOutput decl = filter (`notElem` output decl) $ allVars decl

allVars :: VariableDecl -> [Name]
allVars decl = input decl `union` output decl `union` temp decl

getVarsPat :: Pattern -> [Name]
getVarsPat (QConst _) = []
getVarsPat (QVar n) = [n]
getVarsPat (QPair q1 q2) = getVarsPat q1 `union` getVarsPat q2

getVarsExp :: Expr -> [Name]
getVarsExp (Const _) = []
getVarsExp (Var n) = [n]
getVarsExp (Op _ e1 e2) = getVarsExp e1 `union` getVarsExp e2
getVarsExp (UOp _ e) = getVarsExp e
