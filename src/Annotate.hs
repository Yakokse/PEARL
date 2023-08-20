module Annotate (annotateProg) where

import AST
import AST2
import Division

annotateProg :: Division -> Program a -> Program' a
annotateProg d = map (annotateBlock d)

annotateBlock :: Division -> Block a -> Block' a
annotateBlock d b = Block' 
    { name' = name b
    , from' = annotateFrom d $ from b
    , body' = map (annotateStat d) $ body b
    , goto' = annotateGoto d $ goto b
    }

annotateStat :: Division -> Statement -> Statement'
annotateStat d (Update (Var n) rop e) = 
    case getType n d of
        Static -> 
            case annotateExp d e of
                (e', Static) -> Update' (Var' n) rop e'
                _ -> die
        Dynamic -> 
            Update2 (Var2 n) rop $ lift (annotateExp d e)
annotateStat d (Update (Arr n e1) rop e2) = 
    case getType n d of
        Static -> 
            case (annotateExp d e1, annotateExp d e2) of
                ((e1', Static), (e2', Static)) -> Update' (Arr' n e1') rop e2'
                _ -> die
        Dynamic -> 
            let (e1', e2') = (lift $ annotateExp d e1, lift $ annotateExp d e2) in
            Update2 (Arr2 n e1') rop e2'
annotateStat d (Push n1 n2) =
    case (getType n1 d, getType n2 d) of
        (Static, Static) -> Push' n1 n2
        (Dynamic, Dynamic) -> Push2 n1 n2
        _ -> die
annotateStat d (Pop n1 n2) =
    case (getType n1 d, getType n2 d) of
        (Static, Static) -> Pop' n1 n2
        (Dynamic, Dynamic) -> Pop2 n1 n2
        _ -> die
annotateStat _ Skip = Skip'
                    

annotateFrom :: Division -> IfFrom a -> IfFrom' a
annotateFrom _ Entry = Entry2
annotateFrom _ (From l) = From2 l
annotateFrom d (FromCond e l1 l2) =
    case annotateExp d e of
        (e', Static)  -> FromCond' e' l1 l2
        (e', Dynamic) -> FromCond2 e' l1 l2

annotateGoto :: Division -> IfGoto a -> IfGoto' a
annotateGoto _ Exit = Exit2
annotateGoto _ (Goto l) = Goto2 l
annotateGoto d (GotoCond e l1 l2) =
    case annotateExp d e of
        (e', Static)  -> GotoCond' e' l1 l2
        (e', Dynamic) -> GotoCond2 e' l1 l2

annotateExp :: Division -> Expr -> (Expr', BTtype)
annotateExp _ (Const i) = (Const' i, Static)
annotateExp d (Place (Var n)) = 
    case getType n d of
        Static  -> (Place' (Var' n), Static)
        Dynamic -> (Place' (Var2 n), Dynamic)   
annotateExp d (Place (Arr n e)) =
    case getType n d of
        Static -> 
            case annotateExp d e of
                (e', Static) -> (Place' (Arr' n e'), Static)
                _            -> die
        Dynamic -> (Place' (Arr2 n (lift $ annotateExp d e)), Dynamic)
annotateExp d (Op bop e1 e2) = 
    let ((e1', t1), (e2',t2)) = (annotateExp d e1, annotateExp d e2) in 
    case (t1, t2) of
        (Static, Static) -> (Op' bop e1' e2', Static)
        _ -> (Op2 bop (lift (e1',t1)) (lift (e2',t2)), Dynamic)
annotateExp d (Top n) =
    case getType n d of 
        Static -> (Top' n, Static)
        Dynamic -> (Top2 n, Dynamic)
annotateExp d (Empty n) =
    case getType n d of 
        Static -> (Empty' n, Static)
        Dynamic -> (Empty2 n, Dynamic)

lift :: (Expr', BTtype) -> Expr'
lift (e, Static) = Lift e
lift (e, Dynamic) = e

die :: a
die = undefined -- BTA has failed somehow, should never happen
