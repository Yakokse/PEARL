module Pointwise where

import AST
import Values
import Division
import Utils


-- TODO: Distinguish between patterns

initPWDiv :: NormProgram Label -> Store -> EM (DivisionPW Label)
initPWDiv (decl, prog) store = 
  do d <- makeDiv store decl
     let lStart = nname $ getNEntryBlock prog
         ls = map nname prog
         dstatic = makeStaticDiv decl
         actualDiv l = (l, if l == lStart then dup d else dup dstatic)
         pairs = map actualDiv ls
     return $ listToPWDiv pairs

makeCongruentPW :: Ord a => NormProgram a -> DivisionPW a -> DivisionPW a
makeCongruentPW (_, prog) d = workQueue prog d $ map nname prog 

workQueue :: Ord a => [NormBlock a] -> DivisionPW a -> [a] -> DivisionPW a
workQueue _ pwdiv [] = pwdiv
workQueue prog pwdiv (l:ls) = 
  let (d1, d2) = getDivs l pwdiv
      b = getNBlock prog l
      parentDivs = map (\(l', ()) -> snd $ getDivs l' pwdiv) $ fromLabels $ nfrom b
      newDiv = lubDiv $ d1 : parentDivs
      (d1', d2') = analyseBlock newDiv b
      children = map fst $ jumpLabels $ njump b
      lsNew = if d2' == d2 then [] else children
      pwdivNew = setDiv l (lubDiv [d1, d1'], lubDiv [d2, d2']) pwdiv
  in workQueue prog pwdivNew $ ls ++ lsNew

analyseBlock :: Division -> NormBlock a -> (Division, Division)
analyseBlock d b = analyseStep d $ nstep b

analyseStep :: Division -> Step -> (Division, Division)
analyseStep d (Update n _ e) = dup $ boundedBy n (analyseExpr d e) d
analyseStep d (Replacement q1 q2) = 
  let t = analysePat d q1 `lub` analysePat d q2
      ns = getVarsPat (QPair q1 q2)
      todo = foldl (\d' n -> boundedBy n t d') d ns
  in dup todo
analyseStep d (Assert _) = dup d
analyseStep d Skip = dup d

analysePat :: Division -> Pattern -> Level
analysePat _ (QConst _) = BTStatic
analysePat d (QVar n) = getType n d
analysePat d (QPair q1 q2) = analysePat d q1 `lub` analysePat d q2 

analyseExpr :: Division -> Expr -> Level
analyseExpr _ (Const _)    = BTStatic
analyseExpr d (Var n)      = getType n d
analyseExpr d (Op _ e1 e2) = analyseExpr d e1  `lub` analyseExpr d e2
analyseExpr d (UOp _ e)    = analyseExpr d e
