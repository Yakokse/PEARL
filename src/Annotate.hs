module Annotate (annotateProg) where

import AST
import Division
import Values

annotateProg :: Division -> Program a -> Program' a
annotateProg d (decl, p)= (annotateDecl d decl, map (annotateBlock d) p)

annotateDecl :: Division -> VariableDecl -> VariableDecl'
annotateDecl d VariableDecl{input = i, output = o, temp = t} =
  VariableDecl'{ input' = map annVar i
               , output' = map annVar o
               , temp' = map annVar t}
  where annVar n = (n, getType n d)

annotateBlock :: Division -> Block a -> Block' a
annotateBlock d b = 
  Block' 
    { name' = name b
    , from' = annotateFrom d $ from b
    , body' = map (annotateStep d) $ body b
    , jump' = annotateGoto d $ jump b
    } 
annotateStep :: Division -> Step -> Step'
annotateStep d (Update n rop e) = 
  case getType n d of
    Static -> 
      case annotateExp d e of
        (e', Static) -> Update' Static n rop e'
        _ -> undefined
    Dynamic -> 
      Update' Dynamic n rop $ lift (annotateExp d e)
annotateStep d (Replacement q1 q2) = 
  let btType = patternType d (QPair q1 q2)
  in Replacement' btType q1 q2
annotateStep d (Assert e) = 
  let (e', btType) = annotateExp d e 
  in Assert' btType e'
annotateStep _ Skip = Skip' Static               

annotateFrom :: Division -> ComeFrom a -> ComeFrom' a
annotateFrom _ Entry = Entry'
annotateFrom _ (From l) = From' l
annotateFrom d (Fi e l1 l2) =
  let (e', btType) = annotateExp d e 
  in Fi' btType e' l1 l2

annotateGoto :: Division -> Jump a -> Jump' a
annotateGoto _ Exit = Exit'
annotateGoto _ (Goto l) = Goto' l
annotateGoto d (If e l1 l2) =
  let (e', btType) = annotateExp d e 
  in If' btType e' l1 l2

patternType :: Division -> Pattern -> Level
patternType _ (QConst _) = Static
patternType d (QVar n) = getType n d
patternType d (QPair q1 q2) = 
  case patternType d q1 of
    Static -> patternType d q2
    Dynamic -> Dynamic

annotateExp :: Division -> Expr -> (Expr', Level)
annotateExp _ (Const i) = (Const' Static i, Static)
annotateExp d (Var n) = 
  let btType = getType n d 
  in (Var' btType n, btType)   
annotateExp d (Op bop e1 e2) = 
  let ((e1', t1), (e2',t2)) = (annotateExp d e1, annotateExp d e2) in 
  case (t1, t2) of
    (Static, Static) -> (Op' Static bop e1' e2', Static)
    _ -> (Op' Dynamic bop (lift (e1',t1)) (lift (e2',t2)), Dynamic)
annotateExp d (UOp op e) = 
  let (e', btType) = annotateExp d e 
  in (UOp' btType op e', btType)

lift :: (Expr', Level) -> Expr'
lift (e, Static) = Lift e
lift (e, Dynamic) = e
