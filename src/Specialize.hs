{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Specialize where

import AST
import Values
import Utils
import Operators
import Control.Applicative ((<|>))

type Point a = (a, Store)
type Pending a = [(Point a, Point a)]
type Seen a = Pending a

specialize :: (Eq a, Show a) => Program' a -> Store -> a -> EM (Program (Annotated a))
specialize prog s entry = do 
        b <- getEntry' prog
        let pending = [((b,s), (entry, emptyStore))]
        (_, _, res) <- specProg entry prog pending [] []; return res

specProg :: (Eq a, Show a) => a -> Program' a -> Pending a -> Seen a -> [Block (Annotated a)] 
                            -> EM (Pending a, Seen a, Program (Annotated a))
specProg _ _ [] seen res = return ([], seen, res)
specProg entry prog (p:ps) seen res 
    | p `elem` seen = specProg entry prog ps seen res
    | otherwise = do
        let (current@(l, s), origin) = p
        b <- getBlock' prog l
        (b', p') <- specBlock entry s b origin
        let pnew = map (\x -> (x, current)) p'
        let seen' = p : seen
        res' <- merge b' res
        specProg entry prog (pnew ++ ps) seen' res'

merge :: (Eq a, Show a) => Block (Annotated a) -> Program (Annotated a) 
                            -> EM (Program (Annotated a))
merge block prog 
    | name block `notElem` map name prog = return $ block : prog
    | otherwise = do
        b <- getBlock prog $ name block
        b' <- mergeBlocks b block
        let rest = filter (\x -> name block /= name x) prog
        return $ b' : rest
    where 
        mergeBlocks x y = do j <- mergeFi (from x) (from y); return $ x { from = j }
        mergeFi (FromCond e (l1, s1) (l2, s2)) (FromCond e' (l1', s1') (l2', s2')) 
            | e == e' && l1 == l1' && l2 == l2' = 
                return $ FromCond e (l1, s1 <|> s1') (l2, s2 <|> s2') 
            | otherwise = Left "Failed to merge blocks due to the Fi's being different"
        mergeFi _ _ = Left "Failed to merge blocks due to one or more statement not being a Fi"

specBlock :: (Eq a, Show a) => a -> Store -> Block' a -> (a, Store) 
                                 -> EM (Block (Annotated a), [Point a])
specBlock entry s b origin = do
    let l = (name' b, Just s)
    f <- specFrom entry s (from' b) origin
    (s', as) <- specSteps s $ body' b
    (j, pending) <- specJump s' $ jump' b
    return (Block { name = l, from = f, body = as, jump = j}, pending)

-- TODO: Handle invalid jumps at Spec time or run time, use the annotation?
specFrom :: (Eq a, Show a) => a -> Store -> IfFrom' a -> (a, Store) 
                            -> EM (IfFrom (Annotated a))
specFrom _ _ (From' _ l) origin 
    | l `isFrom` origin = return . From $ annotate l origin
    | otherwise         = Left $ "Invalid jump from " ++ show (fst origin)
specFrom x _ (Entry' _) (l, _) 
    | l == x    = return Entry 
    | otherwise = Left "Invalid jump to entry"
specFrom _ s (FromCond' Elim e l1 l2) origin = do
    v <- getInt e s
    let l = if truthy v then l1 else l2
    if l `isFrom` origin 
        then return . From $ annotate l origin
        else Left $ "Invalid jump during elimination from " ++ show (fst origin)
specFrom _ s (FromCond' Res e l1 l2) origin  
    | l1 `isFrom` origin || l2 `isFrom` origin = do
        e' <- getExpr e s
        return $ FromCond e' (annotate l1 origin) (annotate l2 origin)
    | otherwise = Left $ "Invalid jump from " ++ show (fst origin)

isFrom :: Eq a => a -> (a, Store) -> Bool
isFrom l (l', _) = l == l' -- "l = label(l')" in judgements

annotate :: Eq a => a -> (a, Store) -> Annotated a
annotate l (l', s) | l == l'   = (l, Just s)
                   | otherwise = (l, Nothing)

specJump :: Store -> Jump' a -> EM (Jump (Annotated a), [Point a])
specJump _ (Exit' _) = return (Exit, [])
specJump s (Goto' _ l) = return (Goto (l, Just s), [(l, s)])
specJump s (If' Res e l1 l2) = do
    e' <- getExpr e s; 
    return (If e' (l1, Just s) (l2, Just s), [(l1, s), (l2, s)]) 
specJump s (If' Elim e l1 l2) = do
    v <- getInt e s
    return $ if truthy v 
        then (Goto (l1, Just s), [(l1, s)]) 
        else (Goto (l2, Just s), [(l2, s)])

specSteps :: Store -> [Step'] -> EM (Store, [Step])
specSteps s [] = return (s, [])
specSteps s (a:as) = do
    (s'', a') <- specStep s a
    (s', as') <- specSteps s'' as
    return (s', a' ++ as')
    
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
specStep s (UpdateA' Elim n e1 op e2) = do
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
        _ -> Left $ n ++ " is wrong type (Stack expected)"