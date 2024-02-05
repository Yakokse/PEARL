module Annotate (annotateProg) where

import AST
import Division
import Values
import Utils

annotateProg :: Ord a => DivisionPW a -> Program a () -> Program' a
annotateProg d (_, p)= map (annotateBlock d) p

annotateBlock :: Ord a => DivisionPW a -> Block a () -> Block' a
annotateBlock pwd b = 
  Block' 
    { name' = l
    , from' = annotateFrom d $ from b
    , body' = map (annotateStep d) $ body b
    , jump' = annotateGoto d $ jump b
    } 
  where 
    l = label b
    d = getDiv l pwd

annotateStep :: Division -> Step -> Step'
annotateStep d (Update n rop e) = 
  case getType n d of
    BTStatic -> 
      case annotateExp d e of
        (e', BTStatic) -> Update' BTStatic n rop e'
        _ -> undefined
    BTDynamic -> 
      Update' BTDynamic n rop $ lift (annotateExp d e)
annotateStep d (Replacement q1 q2) = 
  let btType = patternType d (QPair q1 q2)
  in Replacement' btType q1 q2
annotateStep d (Assert e) = 
  let (e', btType) = annotateExp d e 
  in Assert' btType e'
annotateStep _ Skip = Skip' BTStatic               

annotateFrom :: Division -> ComeFrom a () -> ComeFrom' a
annotateFrom _ (Entry ()) = Entry'
annotateFrom _ (From (l, ())) = From' l
annotateFrom d (Fi e (l1, ()) (l2, ())) =
  let (e', btType) = annotateExp d e 
  in Fi' btType e' l1 l2

annotateGoto :: Division -> Jump a () -> Jump' a
annotateGoto _ (Exit ()) = Exit'
annotateGoto _ (Goto (l, ())) = Goto' l
annotateGoto d (If e (l1, ()) (l2, ())) =
  let (e', btType) = annotateExp d e 
  in If' btType e' l1 l2

patternType :: Division -> Pattern -> Level
patternType _ (QConst _) = BTStatic
patternType d (QVar n) = getType n d
patternType d (QPair q1 q2) = 
  case patternType d q1 of
    BTStatic -> patternType d q2
    BTDynamic -> BTDynamic

annotateExp :: Division -> Expr -> (Expr', Level)
annotateExp _ (Const i) = (Const' BTStatic i, BTStatic)
annotateExp d (Var n) = 
  let btType = getType n d 
  in (Var' btType n, btType)   
annotateExp d (Op bop e1 e2) = 
  let ((e1', t1), (e2',t2)) = (annotateExp d e1, annotateExp d e2) in 
  case (t1, t2) of
    (BTStatic, BTStatic) -> (Op' BTStatic bop e1' e2', BTStatic)
    _ -> (Op' BTDynamic bop (lift (e1',t1)) (lift (e2',t2)), BTDynamic)
annotateExp d (UOp op e) = 
  let (e', btType) = annotateExp d e 
  in (UOp' btType op e', btType)

lift :: (Expr', Level) -> Expr'
lift (e, BTStatic) = Lift e
lift (e, BTDynamic) = e
