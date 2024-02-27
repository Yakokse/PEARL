module AnnotateTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Impl.Annotate

import AST
import AST2
import Values
import Division

test :: (Show b, Eq b) => TestName -> (a -> b) -> a -> b -> TestTree
test n f i o = testCase n $ f i @?= o

xyStore :: Level -> Level -> Division
xyStore x y = listToDiv [("x", x), ("y", y)]

tests :: TestTree
tests = testGroup "All Annotation Tests"
  [ expTests
  ]

expTests :: TestTree
expTests = testGroup "Expression Tests"
  [ testExp "Constant" (Const $ Num 1)
            (Const' BTStatic $ Num 1) BTStatic
  , testExp "Static Variable" (Var "x")
            (Var' BTStatic "x") BTStatic
  , testExp "Dynamic Variable" (Var "y")
            (Var' BTDynamic "y") BTDynamic
  , testExp "Static Binop" (Op Mul (Var "x") (Var "x"))
            (Op' BTStatic Mul (Var' BTStatic "x") (Var' BTStatic "x"))
            BTStatic
  , testExp "Dynamic Binop 1" (Op Mul (Var "x") (Var "y"))
            (Op' BTDynamic Mul (Lift $ Var' BTStatic "x") (Var' BTDynamic "y"))
            BTDynamic
  , testExp "Dynamic Binop 2" (Op Mul (Var "y") (Var "y"))
            (Op' BTDynamic Mul (Var' BTDynamic "y") (Var' BTDynamic "y"))
            BTDynamic
  , testExp "Static Unop" (UOp Not (Var "x"))
            (UOp' BTStatic Not (Var' BTStatic "x")) BTStatic
  , testExp "Dynamic Unop" (UOp Not (Var "y"))
            (UOp' BTDynamic Not (Var' BTDynamic "y")) BTDynamic
  ]
  where
    xStat = xyStore BTStatic BTDynamic
    testExp n i = curry $ test n (annotateExp xStat) i
