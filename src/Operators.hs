module Operators where

import AST
import Values

calc :: BinOp -> Value -> Value -> EM Value
calc (ROp r) a b = calcR r a b
calc Mul     a b = 
  do x <- getNum a; y <- getNum b; return . Num $ x * y
calc Div     a b = 
  do x <- getNum a; y <- getNum b; 
     if y /= 0 then return . Num $ x `div` y else Left "Division by 0 error."
calc And     a b = 
  return . boolify $ truthy a && truthy b
calc Or      a b = 
  return . boolify $ truthy a || truthy b
calc Less    a b = 
  do x <- getNum a; y <- getNum b; 
     return . boolify $ x < y 
calc Greater a b = 
  do x <- getNum a; y <- getNum b; 
     return . boolify $ x > y 
calc Equal   a b = 
  return . boolify $ a == b
calc Cons    a b = return $ Pair a b
calc Index   a b = 
  do x <- getNum b; index a x
  where 
    index vs 0 = 
      do (hd, _) <- getPair vs
         return hd
    index vs n =
      do (_, tl) <- getPair vs
         index tl (n-1)

calcR :: RevOp -> Value -> Value -> EM Value
calcR Add a b = 
  do x <- getNum a; y <- getNum b; return . Num $ x + y
calcR Sub a b = 
  do x <- getNum a; y <- getNum b; return . Num $ x - y
calcR Xor a b =
  if a == b then return Nil else do isNil a; return b

calcU :: UnOp -> Value -> EM Value
calcU Hd v = do (hd, _) <- getPair v; return hd
calcU Tl v = do (_, tl) <- getPair v; return tl
calcU Not v = return . boolify . not . truthy $ v

getNum :: Value -> EM IntType
getNum (Num i) = return i
getNum _ = Left "Expected an integer." 

getAtom :: Value -> EM String
getAtom (Atom a) = return a
getAtom _ = Left "Expected an atom." 

getPair :: Value -> EM (Value, Value)
getPair (Pair v1 v2) = return (v1, v2)
getPair _ = Left "Expected a pair." 

isNil :: Value -> EM ()
isNil Nil = return ()
isNil _ = Left "Expected value to be nil."