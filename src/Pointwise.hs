module Pointwise (initPWDiv, makeCongruentPW) where

import AST
import Values
import Division
import Utils

initPWDiv :: Ord a => NormProgram a -> Division -> DivisionPW a
initPWDiv (decl, prog) d = 
  let lStart = nname $ getNEntryBlock prog
      ls = map nname prog
      dstatic = makeStaticDiv decl
      actualDiv l = (l, if l == lStart then (d, dstatic) else dup dstatic)
      pairs = map actualDiv ls
  in listToPWDiv pairs

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
analyseStep d (Replacement left right) = 
  let p2 = analysePat d right
      dMid = setTypes (getVarsPat right) (repeat BTStatic) d
      p1 = analysePat dMid left
      p = p1 `qlub` p2
      d1 = updateTypes p right d
      d2 = updateTypes p left d
  in (d1, d2)
analyseStep d (Assert _) = dup d
analyseStep d Skip = dup d

data BTPattern = QStatic | QDynamic | QCons BTPattern BTPattern
  deriving (Eq, Ord, Show)

updateTypes :: BTPattern -> Pattern -> Division -> Division
updateTypes QStatic p d = 
  foldl (\d' n -> setType n BTStatic d') d $ getVarsPat p
updateTypes QDynamic p d =
  foldl (\d' n -> setType n BTDynamic d') d $ getVarsPat p
updateTypes (QCons q1 q2) (QPair p1 p2) d =
  updateTypes q2 p2 $ updateTypes q1 p1 d
updateTypes q (QVar n) d = setType n (patToLevel q) d
updateTypes _ (QConst _) d = d

qlub :: BTPattern -> BTPattern -> BTPattern
qlub QStatic p = collapsePat p
qlub p QStatic = collapsePat p
qlub QDynamic _ = QDynamic
qlub _ QDynamic = QDynamic
qlub (QCons p1 p2) (QCons p3 p4) = 
  let p1' = p1 `qlub` p3
      p2' = p2 `qlub` p4
  in QCons p1' p2'

collapsePat :: BTPattern -> BTPattern
collapsePat = levelToPat . patToLevel

levelToPat :: Level -> BTPattern
levelToPat BTStatic = QStatic
levelToPat BTDynamic = QDynamic

patToLevel :: BTPattern -> Level
patToLevel QStatic = BTStatic
patToLevel QDynamic = BTDynamic
patToLevel (QCons p1 p2) = patToLevel p1 `lub` patToLevel p2

analysePat :: Division -> Pattern -> BTPattern
analysePat _ (QConst _) = QStatic
analysePat d (QVar n) = levelToPat $ getType n d
analysePat d (QPair q1 q2) = 
  let p1 = analysePat d q1 
      p2 = analysePat d q2
  in QCons p1 p2

analyseExpr :: Division -> Expr -> Level
analyseExpr _ (Const _)    = BTStatic
analyseExpr d (Var n)      = getType n d
analyseExpr d (Op _ e1 e2) = analyseExpr d e1  `lub` analyseExpr d e2
analyseExpr d (UOp _ e)    = analyseExpr d e
