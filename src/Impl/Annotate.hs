module Impl.Annotate where

import AST
import AST2
import Division
import Values

-- Annotate a normalized program
annotateProg :: Ord a => DivisionPW a -> NormProgram a -> Program' a
annotateProg d (_, p)= map (annotateBlock d) p

-- Annotate a normalized block
annotateBlock :: Ord a => DivisionPW a -> NormBlock a -> Block' a
annotateBlock pwd b =
  Block'
    { name' = nname b
    , initDiv = d1
    , from' = annotateFrom d1 $ nfrom b
    , body' = [annotateStep d1 d2 $ nstep b]
    , jump' = annotateJump d2 $ njump b
    }
  where
    (d1, d2) = getDivs (nname b) pwd

-- Annotate a step
annotateStep :: Division -> Division -> Step -> Step'
annotateStep d1 _ (Update n rop e) =
  case getType n d1 of
    BTStatic ->
      case annotateExp d1 e of
        (e', BTStatic) -> Update' BTStatic n rop e'
        _ -> undefined
    BTDynamic ->
      Update' BTDynamic n rop $ lift (annotateExp d1 e)
annotateStep d1 d2 (Replacement q1 q2) =
  let (q1', q2', btType) = annotatePats d2 d1 q1 q2
  in Replacement' btType q1' q2'
annotateStep d1 _ (Assert e) =
  let (e', btType) = annotateExp d1 e
  in Assert' btType e'
annotateStep _ _ Skip = Skip' BTStatic

-- Annotate a come-from
annotateFrom :: Division -> ComeFrom a () -> ComeFrom' a
annotateFrom _ (Entry ()) = Entry'
annotateFrom _ (From (l, ())) = From' l
annotateFrom d (Fi e (l1, ()) (l2, ())) =
  let (e', btType) = annotateExp d e
  in Fi' btType e' l1 l2

-- Annotate a jump
annotateJump :: Division -> Jump a () -> Jump' a
annotateJump _ (Exit ()) = Exit'
annotateJump _ (Goto (l, ())) = Goto' l
annotateJump d (If e (l1, ()) (l2, ())) =
  let (e', btType) = annotateExp d e
  in If' btType e' l1 l2

-- Annotate patterns with precision
annotatePats :: Division -> Division -> Pattern -> Pattern -> (Pattern', Pattern', Level)
annotatePats d1 d2 (QPair q1 q2) (QPair q3 q4) =
  let (q1', q3', t1) = annotatePats d1 d2 q1 q3
      (q2', q4', t2) = annotatePats d1 d2 q2 q4
      t = t1 `lub` t2
  in (QPair' t q1' q2', QPair' t q3' q4', t)
annotatePats d1 d2 q1 q2 =
  let (q1', t1) = annotatePat d1 d2 q1
      (q2', t2) = annotatePat d2 d1 q2
      t = t1 `lub` t2
  in case t of
      BTStatic -> (q1', q2', BTStatic)
      BTDynamic -> (liftQ q1', liftQ q2', BTDynamic)

annotatePat :: Division -> Division -> Pattern -> (Pattern', Level)
annotatePat _ _ (QConst c) = (QConst' BTStatic c, BTStatic)
annotatePat d d' (QVar n) =
  let t = getType n d
      t' = getType n d'
  in case (t, t') of
        (BTDynamic, BTStatic) -> (Drop n, t)
        (_, _) -> (QVar' t n, t)
annotatePat d d' (QPair q1 q2) =
  let (q1', t1) = annotatePat d d' q1
      (q2', t2) = annotatePat d d' q2
      t = t1 `lub` t2
  in (QPair' t q1' q2', t)

-- Annotate an expression
annotateExp :: Division -> Expr -> (Expr', Level)
annotateExp _ (Const i) = (Const' BTStatic i, BTStatic)
annotateExp d (Var n) =
  let btType = getType n d
  in (Var' btType n, btType)
annotateExp d (Op bop e1 e2) =
  let (e1', t1) = annotateExp d e1
      (e2', t2) = annotateExp d e2
  in case t1 `lub` t2 of
    BTStatic -> (Op' BTStatic bop e1' e2', BTStatic)
    BTDynamic -> (Op' BTDynamic bop (lift (e1',t1)) (lift (e2',t2)), BTDynamic)
annotateExp d (UOp op e) =
  let (e', btType) = annotateExp d e
  in (UOp' btType op e', btType)

-- Lift static pattern
liftQ :: Pattern' -> Pattern'
liftQ (QConst' BTStatic c) = QConst' BTDynamic c
liftQ (QPair' BTStatic q1 q2) = QPair' BTDynamic (liftQ q1) (liftQ q2)
liftQ q = q

-- Lift static expressions
lift :: (Expr', Level) -> Expr'
lift (Const' BTStatic c, BTStatic) = Const' BTDynamic c
lift (e, BTStatic) = Lift e
lift (e, BTDynamic) = e
