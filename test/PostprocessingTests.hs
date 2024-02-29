module PostprocessingTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Impl.PostProcessing

import AST

test :: (Show b, Eq b) => (a -> b) -> TestName -> a -> b -> TestTree
test f n i o = testCase n $ f i @?= o

tests :: TestTree
tests = testGroup "All Postprocessing Tests"
  [ emptyBlockTests
  , explicatorTests
  ]

explicatorTests :: TestTree
explicatorTests = testGroup ""
  [undefined]

emptyBlockTests :: TestTree
emptyBlockTests = testGroup "removeEmptyBlocks Tests"
  [ testP "Entry remains" [entryB] [entryB]
  , testP "Exit remains"  [exitB] [exitB]
  , testP "Non-empty remains" [middleBNormal] [middleBNormal]
  , testP "Split remains" [splitB] [splitB]
  , testP "Merge remains" [mergeB] [mergeB]
  , testP "Regular and empty removed" [middleBEmpty] []
  , testP "Correct re-naming of jumps" [entryB, middleBEmpty]
          [entryB{jump = jump middleBEmpty}]
  , testP "Correct re-naming of froms" [middleBEmpty, exitB]
          [exitB{from = from middleBEmpty}]
  , testP "In total" [entryB, middleBEmpty, exitB]
          [ entryB{jump = jump middleBEmpty}
          , exitB{from = from middleBEmpty}]
  , testP "Rename in conditionals"
          [ splitB
          , middleBEmpty{from = From ("split", ()), jump = Goto ("merge", ())}
          , mergeB]
          [ splitB{jump = If (Var "x") ("merge", ()) ("B", ())}
          , mergeB{from = Fi (Var "x") ("split", ()) ("B", ())}]
  ]
  where
    splitB = Block ("split", ()) (From ("B", ())) []
                                 (If (Var "x") ("A", ()) ("B", ()))
    mergeB = Block ("merge", ()) (Fi (Var "x") ("A", ()) ("B", ()))
                    [] (Goto ("B", ()))
    entryB = Block ("init", ()) (Entry ()) [] (Goto ("A", ()))
    exitB = Block ("stop", ()) (From ("A", ())) [] (Exit ())
    middleBNormal = Block ("A", ()) (From ("init", ())) [Skip] (Goto ("stop", ()))
    middleBEmpty = Block ("A", ()) (From ("init", ())) [] (Goto ("stop", ()))
    testP = test removeEmptyBlocks
