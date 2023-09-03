module Annotate (annotateProg) where

import AST
import Division

annotateProg :: Division -> Program a -> Program' a
annotateProg d = map (annotateBlock d)

annotateBlock :: Division -> Block a -> Block' a
annotateBlock d b = 
  Block' 
    { name' = name b
    , from' = annotateFrom d $ from b
    , body' = map (annotateStep d) $ body b
    , jump' = annotateGoto d $ jump b
    } 
annotateStep :: Division -> Step -> Step'
annotateStep d (UpdateV n rop e) = 
  case getType n d of
    Static -> 
      case annotateExp d e of
        (e', Static) -> UpdateV' Elim n rop e'
        _ -> undefined
    Dynamic -> 
      UpdateV' Res n rop $ lift (annotateExp d e)
annotateStep d (UpdateA n e1 rop e2) = 
  case getType n d of
    Static -> 
      case (annotateExp d e1, annotateExp d e2) of
        ((e1', Static), (e2', Static)) -> UpdateA' Elim n e1' rop e2'
        _ -> undefined
    Dynamic -> 
      let (e1', e2') = (lift $ annotateExp d e1, lift $ annotateExp d e2) in
      UpdateA' Res n e1' rop e2'
annotateStep d (Push n1 n2) =
  case (getType n1 d, getType n2 d) of
    (Static, Static) -> Push' Elim n1 n2
    (Dynamic, Dynamic) -> Push' Res n1 n2
    _ -> undefined
annotateStep d (Pop n1 n2) =
  case (getType n1 d, getType n2 d) of
    (Static, Static) -> Pop' Elim n1 n2
    (Dynamic, Dynamic) -> Pop' Res n1 n2
    _ -> undefined
annotateStep _ Skip = Skip' Elim               

annotateFrom :: Division -> IfFrom a -> IfFrom' a
annotateFrom _ Entry = Entry' Res
annotateFrom _ (From l) = From' Res l
annotateFrom d (FromCond e l1 l2) =
  case annotateExp d e of
    (e', Static)  -> FromCond' Elim e' l1 l2
    (e', Dynamic) -> FromCond' Res e' l1 l2

annotateGoto :: Division -> Jump a -> Jump' a
annotateGoto _ Exit = Exit' Res
annotateGoto _ (Goto l) = Goto' Res l
annotateGoto d (If e l1 l2) =
  case annotateExp d e of
    (e', Static)  -> If' Elim e' l1 l2
    (e', Dynamic) -> If' Res e' l1 l2

annotateExp :: Division -> Expr -> (Expr', BTtype)
annotateExp _ (Const i) = (Const' Elim i, Static)
annotateExp d (Var n) = 
  case getType n d of
    Static  -> (Var' Elim n, Static)
    Dynamic -> (Var' Res n, Dynamic)   
annotateExp d (Arr n e) =
  case getType n d of
    Static -> 
      case annotateExp d e of
        (e', Static) -> (Arr' Elim n e', Static)
        _            -> undefined
    Dynamic -> (Arr' Res n (lift $ annotateExp d e), Dynamic)
annotateExp d (Op bop e1 e2) = 
  let ((e1', t1), (e2',t2)) = (annotateExp d e1, annotateExp d e2) in 
  case (t1, t2) of
    (Static, Static) -> (Op' Elim bop e1' e2', Static)
    _ -> (Op' Res bop (lift (e1',t1)) (lift (e2',t2)), Dynamic)
annotateExp d (Top n) =
  case getType n d of 
    Static -> (Top' Elim n, Static)
    Dynamic -> (Top' Res n, Dynamic)
annotateExp d (Empty n) =
  case getType n d of 
    Static -> (Empty' Elim n, Static)
    Dynamic -> (Empty' Res n, Dynamic)

lift :: (Expr', BTtype) -> Expr'
lift (e, Static) = Lift e
lift (e, Dynamic) = e
