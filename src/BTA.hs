module BTA where
import AST
import Division

-- TODO: Expr: static a[dynamic e] needs detection, time for a state monad prob
congruentDiv :: Program a -> Division -> Division
congruentDiv p = fixed $ passProg p
    where fixed f d' | d' == f d' = d'
                     | otherwise = fixed f $ f d'

passProg :: Program a -> Division -> Division
passProg p d = foldl passBlock d p 

passBlock :: Division -> Block a -> Division
passBlock d b = foldl passStep d (body b)

passStep :: Division -> Step -> Division
passStep d (UpdateA n e1 _ e2) 
    | dynamic e1 d || dynamic e2 d = setDyn n d
    | otherwise = d
passStep d (UpdateV n _ e) | dynamic e d = setDyn n d
                           | otherwise   = d
passStep d (Push n a) 
    | isDyn n d || isDyn a d = makeDyn [a,n] d 
    | otherwise   = d
passStep d (Pop n a) 
    | isDyn n d || isDyn a d = makeDyn [a,n] d 
    | otherwise   = d
passStep d Skip = d

dynamic :: Expr -> Division -> Bool
dynamic (Const _) _ = False
dynamic (Var n) d = isDyn n d
dynamic (Arr n e) d = isDyn n d || dynamic e d
dynamic (Op _ e1 e2) d = dynamic e1 d || dynamic e2 d
dynamic (Top n) d = isDyn n d
dynamic (Empty n) d = isDyn n d