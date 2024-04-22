module PostprocessingTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Utils.Maps

import RL.AST
import RL.Values
import PE.AST2
import PE.SpecValues

import PE.Specialization.Impl.PostProcessing

test :: (Show b, Eq b) => (a -> b) -> TestName -> a -> b -> TestTree
test f n i o = testCase n $ f i @?= o

tests :: TestTree
tests = testGroup "All Postprocessing Tests"
  [ emptyBlockTests
  , explicatorTests
  ]

-- Check that appropriate amount of blocks generated
explicatorTests :: TestTree
explicatorTests = testGroup "Explicator Tests" $
  [ testExpl "Single Expl" [explX11] 1
  , testExpl "Two labels, different store" [explX22, explY] 2
  , testExpl "Two labels, partially different" [explX12, explY] 2
  , testExpl "Two labels, same store" [explX11, explY] 2
  , testExpl "One label, different stores" [explX11, explX22] 2
  , testExpl "One label, disagree on explicator only" [explX11, explX21] 3
  , testExpl "One label, disagree on other only" [explX11, explX12] 2
  ] ++ map testExpls [3, 4, 7, 8, 9, 31, 32, 33, 512]
  where
    merge :: String -> Int -> Int -> String
    merge = const . const
    createStore a b = fromList [("x", Static $ Num a), ("y", Static $ Num b)]
    createExpl l xs s = Block (Explicator l xs, s)
                              (From (Regular "A", emptyMap))
                              []
                              (Goto (Regular "B", emptyMap))
    explX11 = createExpl "explX" ["x"] $ createStore 1 1
    explX12 = createExpl "explX" ["x"] $ createStore 1 2
    explX21 = createExpl "explX" ["x"] $ createStore 2 1
    explX22 = createExpl "explX" ["x"] $ createStore 2 2
    explY   = createExpl "explY" ["y"] $ createStore 1 1
    testExpl = test (length . mergeExplicators merge)
    expls n = map (\i -> createExpl "explX" ["x"] $ createStore i 1) [1..n]
    testExpls n =
      testExpl ("Larger merge (" ++ show n ++")") (expls n)
               (fromEnum n + fromEnum n - 1)


emptyBlockTests :: TestTree
emptyBlockTests = testGroup "removeEmptyBlocks Tests"
  [ testP "Entry remains" [entryB] [entryB]
  , testP "Exit remains"  [exitB] [exitB]
  , testP "Non-empty remains" [middleBNormal] [middleBNormal]
  , testP "Split remains" [splitB] [splitB]
  , testP "Merge remains" [mergeB] [mergeB]
  , testP "Removeable and correct renaming" [entryB, middleBEmpty, exitB]
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
