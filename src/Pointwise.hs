module Pointwise where

import AST
import Values ( lub, EM, Label, Level(..), Name, Store )
import Division
import Utils
import Data.Maybe (fromJust)

-- todo: Operate on normalized?

initPWDiv :: Program Label () -> Store -> EM (DivisionPW Label)
initPWDiv (decl, prog) store = 
  do d <- makeDiv store decl
     let ls = labels prog
         pairs = map (\l -> (l, d)) ls
     return $ listToPWDiv pairs

makeCongruentPW :: Ord a => Program a () -> DivisionPW a -> DivisionPW a
makeCongruentPW (_, prog) d = workQueue prog d $ labels prog 

workQueue :: Ord a => [Block a ()] -> DivisionPW a -> [a] -> DivisionPW a
workQueue _ pwdiv [] = pwdiv
workQueue prog pwdiv (l:ls) = 
  let initDiv = getDiv l pwdiv
      b = fromJust $ getBlock prog (l, ())
      parentDivs = map (\(l', ()) -> getDiv l' pwdiv) $ fromLabels b
      newDiv = lubDiv $ initDiv : parentDivs
      resultDiv = analyseBlock newDiv b
      children = map fst $ jumpLabels b
      lsNew = if resultDiv == initDiv then [] else children
      pwdivNew = setDiv l resultDiv pwdiv
  in workQueue prog pwdivNew $ ls ++ lsNew

analyseBlock :: Division -> Block a () -> Division
analyseBlock d b = foldl analyseStep d $ body b

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
