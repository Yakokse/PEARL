module Impl.Pointwise where

import AST
import Division
import Utils
import Impl.Maps
import Impl.SpecValues

-- create initial PW division for a given starting division
-- all divisions are fully static except entry block
initPWDiv :: Ord a => NormProgram a -> Division -> PWDivision a
initPWDiv (decl, prog) d =
  let lStart = nname $ getNEntryBlock prog
      ls = map nname prog
      dstatic = makeStaticDiv decl
      actualDiv l = (l, if l == lStart then (d, dstatic) else dup dstatic)
      pairs = map actualDiv ls
  in fromList pairs

-- make PW division congruent
makeCongruentPW :: Ord a => NormProgram a -> PWDivision a -> PWDivision a
makeCongruentPW (_, prog) d = workQueue prog d $ map nname prog

-- fix-point iteration powered by a work-queue
workQueue :: Ord a => [NormBlock a] -> PWDivision a -> [a] -> PWDivision a
workQueue _ pwdiv [] = pwdiv
workQueue prog pwdiv (l:ls) =
  let (d1, d2) = get l pwdiv
      b = getNBlock prog l
      parentDivs = map (\(l', ()) -> snd $ get l' pwdiv) $ fromLabels $ nfrom b
      newDiv = lubDiv $ d1 : parentDivs
      (d1', d2') = analyseBlock newDiv b
      children = map fst $ jumpLabels $ njump b
      lsNew = if d2' == d2 then [] else children
      pwdivNew = set l (lubDiv [d1, d1'], lubDiv [d2, d2']) pwdiv
  in workQueue prog pwdivNew $ ls ++ lsNew

-- analyse a normalized block
-- ignore control-flow since it does not affect divisions
-- returns initial and final division of a block
analyseBlock :: Division -> NormBlock a -> (Division, Division)
analyseBlock d b = analyseStep d $ nstep b

-- analyse a single step
-- most are uniform
analyseStep :: Division -> Step -> (Division, Division)
analyseStep d (Update n _ e) = dup $ boundedBy n (analyseExpr d e) d
analyseStep d (Replacement left right) =
      -- get the extended BTType for the RHS Pattern
  let pRight = analysePat d right
      varsRight = getVarsPat right
      -- get the extended BTType for the LHS Pattern
      -- Variables from the RHS pattern are considered static
      -- since theyve been nil-cleared
      dTemp = sets varsRight (repeat BTStatic) d
      pLeft = analysePat dTemp left
      -- Get the least upper bound of the two extended types
      p = pRight `qlub` pLeft
      -- Use this information to set the respective variables in each pattern
      dRight = updateTypes p right d
      dLeft = updateTypes p left dTemp
  in (dRight, dLeft)
analyseStep d (Assert _) = dup d
analyseStep d Skip = dup d

data BTPattern = QStatic | QDynamic | QCons BTPattern BTPattern
  deriving (Eq, Ord, Show)

updateTypes :: BTPattern -> Pattern -> Division -> Division
updateTypes QStatic p d =
  sets (getVarsPat p) (repeat BTStatic) d
updateTypes QDynamic p d =
  sets (getVarsPat p) (repeat BTDynamic) d
updateTypes (QCons q1 q2) (QPair p1 p2) d =
  updateTypes q2 p2 $ updateTypes q1 p1 d
updateTypes q (QVar n) d = set n (patToLevel q) d
updateTypes _ (QConst _) d = d

-- least-upper-bound of two expanded levels
qlub :: BTPattern -> BTPattern -> BTPattern
qlub QStatic p = collapsePat p
qlub p QStatic = collapsePat p
qlub QDynamic _ = QDynamic
qlub _ QDynamic = QDynamic
qlub (QCons p1 p2) (QCons p3 p4) =
  let p1' = p1 `qlub` p3
      p2' = p2 `qlub` p4
  in QCons p1' p2'

-- collapse expanded level into expanded level
collapsePat :: BTPattern -> BTPattern
collapsePat = levelToPat . patToLevel

-- expand level into expanded level
levelToPat :: Level -> BTPattern
levelToPat BTStatic = QStatic
levelToPat BTDynamic = QDynamic

-- Collapse expanded level into level
patToLevel :: BTPattern -> Level
patToLevel QStatic = BTStatic
patToLevel QDynamic = BTDynamic
patToLevel (QCons p1 p2) = patToLevel p1 `lub` patToLevel p2

-- find expanded level of a pattern under a division
analysePat :: Division -> Pattern -> BTPattern
analysePat _ (QConst _) = QStatic
analysePat d (QVar n) = levelToPat $ get n d
analysePat d (QPair q1 q2) =
  let p1 = analysePat d q1
      p2 = analysePat d q2
  in QCons p1 p2

-- find level of expression under a division
analyseExpr :: Division -> Expr -> Level
analyseExpr _ (Const _)    = BTStatic
analyseExpr d (Var n)      = get n d
analyseExpr d (Op _ e1 e2) = analyseExpr d e1  `lub` analyseExpr d e2
analyseExpr d (UOp _ e)    = analyseExpr d e
