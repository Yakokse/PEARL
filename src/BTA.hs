module BTA where
import AST
import Division

congruentDiv :: Program a -> Division -> Division
congruentDiv p = fixed $ passProg p
    where fixed f d' | d' == f d' = d'
                     | otherwise = fixed f $ f d'

passProg :: Program a -> Division -> Division
passProg p d = foldl passBlock d p 

passBlock :: Division -> Block a -> Division
passBlock d b = foldl passStep d (body b)

passStep :: Division -> Step -> Division
passStep d (UpdateA n e _ _) | dynamic e d = setDyn n d
passStep d (UpdateA n _ _ e) | dynamic e d = setDyn n d
passStep d (UpdateV n _ e)   | dynamic e d = setDyn n d
passStep d (Push n a)        | isDyn n d   = setDyn a d 
passStep d (Push n a)        | isDyn a d   = setDyn n d
passStep d (Pop n a)         | isDyn n d   = setDyn a d 
passStep d (Pop n a)         | isDyn a d   = setDyn n d
passStep d Skip = d

dynamic :: Expr -> Division -> Bool
dynamic (Const _) _ = False
dynamic (Var n) d = isDyn n d
dynamic (Arr n e) d = isDyn n d || dynamic e d
dynamic (Op _ e1 e2) d = dynamic e1 d || dynamic e2 d
dynamic (Top n) d = isDyn n d
dynamic (Empty n) d = isDyn n d