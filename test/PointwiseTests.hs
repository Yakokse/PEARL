module PointwiseTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Utils.Maps

import RL.AST

import PE.SpecValues
import PE.Preprocessing.Division


import PE.Preprocessing.Impl.Pointwise


testStep :: TestName -> Step -> Division -> (Division, Division) -> TestTree
testStep n s d o = testCase n $ analyseStep d s @?= o

xyStore :: Level -> Level -> Division
xyStore x y = fromList [("x", x), ("y", y)]

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
  [ testIn "S1 <- S2" allStat moveXY allStat allStat
  , testIn "D <- S"   xStat   moveXY allDyn  xStat
  , testIn "S <- D"   yStat   moveXY yStat   xStat
  , testIn "D1 <- D2" allDyn  moveXY allDyn  xStat
  , testIn "S1 <- (S2 . S1)" allStat pushXY allStat allStat
  , testIn "D <- (S . D)"    xStat   pushXY allDyn  xStat
  , testIn "S <- (D . S)"    yStat   pushXY allDyn  xStat
  , testIn "D1 <- (D2 . D1)" allDyn  pushXY allDyn  xStat
  , testIn "(S1 . S2) <- S2" allStat popXY  allStat allStat
  , testIn "(S . D) <- D"    xStat   popXY  xStat   allDyn
  , testIn "(D . S) <- D"    yStat   popXY  allDyn  allDyn
  , testIn "(D1 . D2) <- D2" allDyn  popXY  allDyn  allDyn
  , testIn "(S1 . S2) <- (S2 . S1)" allStat swapXY allStat allStat
  , testIn "(S . D) <- (D . S)"     xStat   swapXY xStat   yStat
  , testIn "(D . S) <- (S . D)"     yStat   swapXY yStat   xStat
  , testIn "(D1 . D2) <- (D2 . D1)" allDyn  swapXY allDyn  allDyn
  ]
  where
    allStat = xyStore BTStatic BTStatic
    xStat = xyStore BTStatic BTDynamic
    yStat = xyStore BTDynamic BTStatic
    allDyn = xyStore BTDynamic BTDynamic
    testIn n i s o1 o2 = testStep n s i (o1, o2)
    moveXY = Replacement (QVar "y") (QVar "x")
    pushXY = Replacement (QVar "y") (QPair (QVar "x") (QVar "y"))
    popXY = Replacement (QPair (QVar "x") (QVar "y")) (QVar "y")
    swapXY = Replacement (QPair (QVar "x") (QVar "y"))
                         (QPair (QVar "y") (QVar "x"))
