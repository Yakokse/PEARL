module Specialize where

import AST
import AST2
import RWSE
import Values
import Utils
import Operators

type ReadData a = Program' a
type StateSet a = [(a, Store)]
-- type WriteData a = (StateSet a, Program a)  -- must be an instance of Monoid


-- RWSE READ STATE WRITE VAL
type ProgSpec a = RWSE (ReadData a) (StateSet a) (Program (a, Store))
-- type BlockSpec a = RWSE (ReadData a) Store (WriteData (a, Store))

specialize :: Program' a -> Store -> EM (Program (a, Store))
specialize p s = do 
    entry <- findEntry' p
    (_, x, _) <- runRWSE specProg  p [(entry, s)]; return x



specProg :: ProgSpec a ()
specProg = undefined

specFrom :: Store -> IfFrom' a -> (a, Store) -> IfFrom (a, Store)
specFrom s _ _ = undefined 

specGoto :: Store -> IfGoto' a -> EM (IfGoto (a, Store), [a])
specGoto _ (Exit' _) = return (Exit, [])
specGoto s (Goto' _ l) = return (Goto (l, s), [l])
specGoto s (GotoCond' Res e l1 l2) = do
    e' <- getExpr e s; 
    return (GotoCond e' (l1, s) (l2, s), [l1, l2]) 
specGoto s (GotoCond' Elim e l1 l2) = do
    v <- getInt e s
    return $ if truthy v 
        then (Goto (l1, s), [l1]) 
        else (Goto (l2, s), [l2])

specStep :: Store -> Step' -> EM (Store, [Step])
specStep s (Skip' Res) = return (s, [Skip])
specStep s (Skip' Elim) = return (s, [])
specStep s (Push' Res n1 n2) = return (s, [Push n1 n2])
specStep s (Push' Elim n1 n2) = do
    i <- getVarScalar n1 s
    stack <- getVarStack n2 s
    let s' = update n1 (ScalarVal 0) s 
        s'' = update n2 (StackVal (i:stack)) s' in 
        return (s'', [])
specStep s (Pop' Res n1 n2) = return (s, [Pop n1 n2])
specStep s (Pop' Elim n1 n2) = do
    i <- getVarScalar n1 s
    stack <- getVarStack n2 s
    case (i, stack) of
        (0, x:xs) -> let s' = update n1 (ScalarVal x) s 
                         s'' = update n2 (StackVal xs) s' in 
                         return (s'', [])
        (0, _) -> Left $ n2 ++ " is empty when trying to pop."
        _ -> Left $ n1 ++ " is expected to be 0 but is actually " ++ show i ++ "."
specStep s (UpdateV' Res n op e) = do
    e' <- getExpr e s
    return (s, [UpdateV n op e'])
specStep s (UpdateV' Elim n op e) = do
    i <- getInt e $ s `without` n
    v <- getVarScalar n s
    let s' = update n (ScalarVal $ calcR op v i) s in
        return (s', [])
specStep s (UpdateA' Res n e1 op e2) = do
    e1' <- getExpr e1 s; e2' <- getExpr e2 s
    return (s, [UpdateA n e1' op e2'])
specStep s (UpdateA' Elim n e1 op e2) = do -- NOTE: Strengthened restriction
    i1 <- getInt e1 $ s `without` n
    i2 <- getInt e2 $ s `without` n
    arr <- getVarArr n s
    let val = calcR op (arr ! i1) i2
        arr' = updateIdx arr i1 val in
        return (update n (ArrVal arr') s, [])

type PEValue = Either IntType Expr

specExpr :: Store -> Expr' -> EM PEValue
specExpr _ (Const' Res x) = return . Right $ Const x
specExpr _ (Const' Elim x) = return . Left $ x
specExpr _ (Var' Res n) = return . Right $ Var n
specExpr s (Var' Elim n) = do v <- getVarScalar n s; return . Left $ v
specExpr s (Arr' Res n e) = do e' <- getExpr e s; return . Right $ Arr n e'
specExpr s (Arr' Elim n e) = do i <- getInt e s; a <- getVarArr n s; return . Left $ a ! i
specExpr _ (Top' Res n) = return . Right $ Top n
specExpr s (Top' Elim n) = do
    v <- getVarStack n s
    case v of 
        (x:_) -> return . Left $ x
        [] -> Left $ n ++ " is empty."
specExpr _ (Empty' Res n) = return . Right $ Empty n
specExpr s (Empty' Elim n) = do
    v <- getVarStack n s
    case v of 
        (_:_) -> return . Left $ 0
        [] -> return . Left $ 1
specExpr s (Op' Res op e1 e2) = do 
    e1' <- getExpr e1 s; e2' <- getExpr e2 s
    return . Right $ Op op e1' e2'
specExpr s (Op' Elim op e1 e2) = do
    v1 <- getInt e1 s; v2 <- getInt e2 s
    case (op, v2) of
        (Div, 0) -> Left "Division by 0 error"
        _ -> return . Left $ calc op v1 v2
specExpr s (Lift e) = do i <- getInt e s; return . Right $ Const i

getExpr ::  Expr' -> Store -> EM Expr
getExpr e s = do
    res <- specExpr s e
    case res of
        Right e' -> return e'
        _ -> Left "Eliminable code found where residual was expected."

getInt ::  Expr' -> Store -> EM IntType
getInt e s = do
    res <- specExpr s e 
    case res of
        Left i -> return i
        _ -> Left "Residual code found where eliminable code was expected."

getVarVal :: Name -> Store -> EM Value
getVarVal n s =
    case find n s of
        Just x -> return x
        Nothing -> Left $ n ++ " not found."

getVarScalar :: Name -> Store -> EM IntType
getVarScalar n s = do
    v <- getVarVal n s
    case v of
        ScalarVal x -> return x
        _ -> Left $ n ++ " is wrong type (Scalar expected)"

getVarArr :: Name -> Store -> EM ArrayType
getVarArr n s = do
    v <- getVarVal n s
    case v of
        ArrVal x -> return x
        _ -> Left $ n ++ " is wrong type (Array expected)"

getVarStack :: Name -> Store -> EM StackType
getVarStack n s = do
    v <- getVarVal n s
    case v of
        StackVal x -> return x
        _ -> Left $ n ++ " is wrong type (STack expected)"