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
passBlock d b = foldl passStat d (body b)

passStat :: Division -> Statement -> Division
passStat d (Update (Arr n e) _ _) | dynamic e d = setDyn n d
passStat d (Update p _ e)         | dynamic e d = setDyn (nameOf p) d
passStat d (Push n a)             | isDyn n d   = setDyn a d 
passStat d (Push n a)             | isDyn a d   = setDyn n d
passStat d (Pop n a)              | isDyn n d   = setDyn a d 
passStat d (Pop n a)              | isDyn a d   = setDyn n d
passStat d _ = d

dynamic :: Expr -> Division -> Bool
dynamic (Const _) _ = False
dynamic (Place p) d = nameOf p `isDyn` d
dynamic (Op _ e1 e2) d = dynamic e1 d || dynamic e2 d
dynamic (Top n) d = n `isDyn` d
dynamic (Empty n) d = n `isDyn` d

nameOf :: Place -> Name 
nameOf (Var n) = n
nameOf (Arr n _) = n