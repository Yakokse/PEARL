module PointwiseTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Impl.Pointwise

import AST
import Values
import Division

testStep :: TestName -> Step -> Division -> (Division, Division) -> TestTree
testStep n s i o = testCase n $ analyseStep i s @?= o

xyStore :: Level -> Level -> Division
xyStore x y = listToDiv [("x", x), ("y", y)]

tests :: TestTree
tests = testGroup "All Uniform BTA Tests"
  [ replacementTests
  , updateTests
  ]

updateTests :: TestTree
updateTests = testGroup "Reversible Update Tests"
  [ testIn "Static with static" allStat allStat
  , testIn "Dynamic with static" yStat yStat
  , testIn "Static with dynamic" xStat allDyn
  , testIn "Dynamic with dynamic" allDyn allDyn
  ]
  where
    allStat = xyStore BTStatic BTStatic
    xStat = xyStore BTStatic BTDynamic
    yStat = xyStore BTDynamic BTStatic
    allDyn = xyStore BTDynamic BTDynamic
    testIn  n i o = testStep n step i (o, o)
    step = Update "x" Add (Var "y")

replacementTests :: TestTree
replacementTests = testGroup "Reversible Replacement Tests"
  [ testIn "Move Static"     allStat moveXY allStat allStat
  , testIn "Move (Statix x)" xStat   pushXY allDyn  allDyn
  , testIn "Move (Statix y)" yStat   pushXY allDyn  allDyn
  , testIn "Move Dynamic"    allDyn  pushXY allDyn  allDyn
  , testIn "Push Static"     allStat pushXY allStat allStat
  , testIn "Push (Statix x)" xStat   pushXY allDyn  allDyn
  , testIn "Push (Statix y)" yStat   pushXY allDyn  allDyn
  , testIn "Push Dynamic"    allDyn  pushXY allDyn  allDyn
  , testIn "Pop Static"      allStat popXY  allStat allStat
  , testIn "Pop (Statix x)"  xStat   popXY  allDyn  allDyn
  , testIn "Pop (Statix y)"  yStat   popXY  allDyn  allDyn
  , testIn "Pop Dynamic"     allDyn  popXY  allDyn  allDyn
  , testIn "Swap Static"     allStat swapXY allStat allStat
  , testIn "Swap (Statix x)" xStat   swapXY xStat   yStat
  , testIn "Swap (Statix y)" yStat   swapXY yStat   xStat
  , testIn "Swap Dynamic"    allDyn  swapXY allDyn  allDyn
  ]
  where
    allStat = xyStore BTStatic BTStatic
    xStat = xyStore BTStatic BTDynamic
    yStat = xyStore BTDynamic BTStatic
    allDyn = xyStore BTDynamic BTDynamic
    testIn  n i s o1 o2 = testStep n s i (o1, o2)
    moveXY = Replacement (QVar "y") (QVar "x")
    pushXY = Replacement (QVar "y") (QPair (QVar "x") (QVar "y"))
    popXY = Replacement (QPair (QVar "x") (QVar "y")) (QVar "y")
    swapXY = Replacement (QPair (QVar "x") (QVar "y"))
                         (QPair (QVar "y") (QVar "x"))
