module Operators where

import AST
import Values
import GHC.Bits


calc :: BinOp -> IntType -> IntType -> IntType
calc (ROp r) n m = calcR r n m
calc Mul n m     = n * m
calc Div n m     = n `div` m
calc And n m     = boolify $ truthy n && truthy m
calc Or n m      = boolify $ truthy n || truthy m
calc Less n m    = boolify $ n < m
calc Greater n m = boolify $ n > m
calc Equal n m   = boolify $ n == m

calcR :: RevOp -> IntType -> IntType -> IntType
calcR Add n m = n + m
calcR Sub n m = n - m
calcR Xor n m = xor n m
