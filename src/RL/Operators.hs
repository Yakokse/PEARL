module Operators where

import AST
import Values

-- definition of binary operators
calc :: BinOp -> Value -> Value -> EM Value
calc (ROp r) a b = calcR r a b
calc Mul     a b =
  do x <- getNum a; y <- getNum b; return . Num $ x * y
calc Div     a b =
  do x <- getNum a; y <- getNum b;
     if y /= 0 then return . Num $ x `div` y else Left "Division by 0 error."
calc And     a b =
  return $ if truthy a then b else a
calc Or      a b =
  return $ if truthy a then a else b
calc Less    a b =
  do x <- getNum a; y <- getNum b;
     return $ if x < y then Num y else falseV
calc Greater a b =
  do x <- getNum a; y <- getNum b;
     return $ if x > y then Num y else falseV
calc Equal   a b =
  return . boolify $ a == b
calc Cons    a b = return $ Pair a b

-- definition of reversible binary operators
calcR :: RevOp -> Value -> Value -> EM Value
calcR Add a b =
  do x <- getNum a; y <- getNum b; return . Num $ x + y
calcR Sub a b =
  do x <- getNum a; y <- getNum b; return . Num $ x - y
calcR Xor a b
  | a == b    = return Nil
  | a == Nil  = return b
  | b == Nil  = return a
  | otherwise = Left "Xor on non-matching elements"
  -- if a == b then return Nil else do isNil a; return b

-- definition of unary operators
calcU :: UnOp -> Value -> EM Value
calcU Hd v = do (hd, _) <- getPair v; return hd
calcU Tl v = do (_, tl) <- getPair v; return tl
calcU Not v = return . boolify . not . truthy $ v

-- Make a value into an int
getNum :: Value -> EM IntType
getNum (Num i) = return i
getNum _ = Left "Expected an integer."

-- make a value into two values
getPair :: Value -> EM (Value, Value)
getPair (Pair v1 v2) = return (v1, v2)
getPair _ = Left "Expected a pair."
