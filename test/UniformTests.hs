module UniformTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Utils.Maps

import RL.AST

import PE.SpecValues
import PE.Preprocessing.Division

import Control.Monad.State

import PE.Preprocessing.Impl.Uniform


test :: (VariableDecl -> a -> ST b) ->
        TestName ->
        VariableDecl ->
        Division -> a -> Division -> TestTree
test f n decl di i o = testCase n $ getDiv (f decl i) di @?= o

xyStore :: Level -> Level -> Division
xyStore x y = fromList [("x", x), ("y", y)]

getDiv :: ST a -> Division-> Division
getDiv = execState

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
  , testOut "Static but output" ["x"] yStat
  , testOut "Body in output" ["y"] allStat
  ]
  where
    allStat = xyStore BTStatic BTStatic
    xStat = xyStore BTStatic BTDynamic
    yStat = xyStore BTDynamic BTStatic
    allDyn = xyStore BTDynamic BTDynamic
    testIn  n d = test checkStep n (outdecl []) d step
    testOut n ns = test checkStep n (outdecl ns) allStat step
    outdecl ns = VariableDecl [] ns []
    step = Update "x" Add (Var "y")

replacementTests :: TestTree
replacementTests = testGroup "Reversible Replacement Tests"
  [ testIn "Push XY Static"    allStat       pushXY allStat
  , testIn "Push XY Dynamic 1" partialStat1  pushXY allDyn
  , testIn "Push XY Dynamic 2" partialStat2  pushXY allDyn
  , testIn "Push XY Dynamic 3" allDyn        pushXY allDyn
  , testIn "Pop XY Static"     allStat       popXY  allStat
  , testIn "Pop XY Dynamic 1"  partialStat1  popXY  allDyn
  , testIn "Pop XY Dynamic 2"  partialStat2  popXY  allDyn
  , testIn "Pop XY Dynamic 3"  allDyn        popXY  allDyn
  , testIn "Swap XY Static"    allStat       swapXY allStat
  , testIn "Swap XY Dynamic 1" partialStat1  swapXY allDyn
  , testIn "Swap XY Dynamic 2" partialStat2  swapXY allDyn
  , testIn "Swap XY Dynamic 3" allDyn        swapXY allDyn
  , testOut "Push static out 1" ["x"] pushXY allDyn
  , testOut "Push static out 2" ["y"] pushXY allDyn
  , testOut "Pop static out 1" ["x"]  popXY  allDyn
  , testOut "Pop static out 2" ["y"]  popXY  allDyn
  , testOut "Swap static out 1" ["x"] swapXY allDyn
  , testOut "Swap static out 2" ["y"] swapXY allDyn
  ]
  where
    allStat = xyStore BTStatic BTStatic
    partialStat1 = xyStore BTStatic BTDynamic
    partialStat2 = xyStore BTDynamic BTStatic
    allDyn = xyStore BTDynamic BTDynamic
    testIn  n = test checkStep n $ outdecl []
    testOut n ns = test checkStep n (outdecl ns) allStat
    outdecl ns = VariableDecl [] ns []
    pushXY = Replacement (QVar "y") (QPair (QVar "x") (QVar "y"))
    popXY = Replacement (QPair (QVar "x") (QVar "y")) (QVar "y")
    swapXY = Replacement (QPair (QVar "x") (QVar "y"))
                         (QPair (QVar "y") (QVar "x"))
