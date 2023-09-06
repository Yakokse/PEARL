module Operators where

import AST
import Values
import GHC.Bits


calc :: BinOp -> IntType -> IntType -> Maybe IntType
calc (ROp r) n m = return $ calcR r n m
calc Mul n m     = return $ n * m
calc Div _ 0     = Nothing
calc Div n m     = return $ n `div` m
calc And n m     = return . boolify $ truthy n && truthy m
calc Or n m      = return . boolify $ truthy n || truthy m
calc Less n m    = return . boolify $ n < m
calc Greater n m = return . boolify $ n > m
calc Equal n m   = return . boolify $ n == m

calcR :: RevOp -> IntType -> IntType -> IntType
calcR Add n m = n + m
calcR Sub n m = n - m
calcR Xor n m = xor n m
