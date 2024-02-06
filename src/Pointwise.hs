module Pointwise where

import AST
import Values
import Division
import Utils


-- TODO: Distinguish between patterns

initPWDiv :: NormProgram Label -> Store -> EM (DivisionPW Label)
initPWDiv (decl, prog) store = 
  do d <- makeDiv store decl
     let ls = map nname prog
         pairs = map (\l -> (l, d)) ls
     return $ listToPWDiv pairs

makeCongruentPW :: Ord a => NormProgram a -> DivisionPW a -> DivisionPW a
makeCongruentPW (_, prog) d = workQueue prog d $ map nname prog 

workQueue :: Ord a => [NormBlock a] -> DivisionPW a -> [a] -> DivisionPW a
workQueue _ pwdiv [] = pwdiv
workQueue prog pwdiv (l:ls) = 
  let initDiv = getDiv l pwdiv
      b = getNBlock prog l
      parentDivs = map (\(l', ()) -> getDiv l' pwdiv) $ fromLabels $ nfrom b
      newDiv = lubDiv $ initDiv : parentDivs
      resultDiv = analyseBlock newDiv b
      children = map fst $ jumpLabels $ njump b
      lsNew = if resultDiv == initDiv then [] else children
      pwdivNew = setDiv l resultDiv pwdiv
  in workQueue prog pwdivNew $ ls ++ lsNew

analyseBlock :: Division -> NormBlock a -> Division
analyseBlock d b = analyseStep d $ nstep b

analyseStep :: Division -> Step -> Division
analyseStep d (Update n _ e) = boundBy n (analyseExpr d e) d
analyseStep d (Replacement q1 q2) = 
  let t = analysePat d q1 `lub` analysePat d q2
      ns = getVarsPat (QPair q1 q2)
  in foldl (\d' n -> boundBy n t d') d ns
analyseStep d (Assert _) = d
analyseStep d Skip = d

analysePat :: Division -> Pattern -> Level
analysePat _ (QConst _) = BTStatic
analysePat d (QVar n) = getType n d
analysePat d (QPair q1 q2) = analysePat d q1 `lub` analysePat d q2 

analyseExpr :: Division -> Expr -> Level
analyseExpr _ (Const _)    = BTStatic
analyseExpr d (Var n)      = getType n d
analyseExpr d (Op _ e1 e2) = analyseExpr d e1  `lub` analyseExpr d e2
analyseExpr d (UOp _ e)    = analyseExpr d e
