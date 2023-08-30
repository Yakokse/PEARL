module Operators where

import AST
import Values
import GHC.Bits


calc :: BinOp -> IntType -> IntType -> IntType
calc (ROp r) n m = calcR r n m
calc Mul n m = n * m
calc Div n m = n `div` m
calc And n m = if truthy n && truthy m then trueV else falseV
calc Or n m = if truthy n || truthy m then trueV else falseV

calcR :: RevOp -> IntType -> IntType -> IntType
calcR Add n m = n + m
calcR Sub n m = n - m
calcR Xor n m = xor n m
