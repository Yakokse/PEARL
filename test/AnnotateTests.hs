module AnnotateTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Impl.Annotate

import AST
import Values 
import Division

test :: (Show b, Eq b) => TestName -> (a -> b) -> a -> b -> TestTree
test n f i o = testCase n $ f i @?= o

xyStore :: Level -> Level -> Division
xyStore x y = listToDiv [("x", x), ("y", y)]

tests :: TestTree
tests = testGroup "All Uniform BTA Tests" 
  [ expTests
  ]

expTests :: TestTree
expTests = testGroup "Expression Tests"
  [

  ]
  where 
    xStat = xyStore BTStatic BTDynamic
    testExp n d i = curry $ test n (annotateExp d) i 
    

