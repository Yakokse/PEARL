module WellformedTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import RL.Impl.Wellformed
import RL.AST
import Values

wellformed :: (a -> EM b) -> TestName -> a -> TestTree
wellformed f n x = testCase n $ case f x of Right _ -> return (); Left e -> assertFailure e

malformed :: (a -> EM b) -> TestName -> a -> TestTree
malformed f n x = testCase n $ case f x of Left _ -> return (); _ -> assertFailure "Unexpectedly wellformed"

tests :: TestTree
tests = testGroup "All Inversion Tests"
  [ exprTests
  , patTests
  , stepTests
  , jumpTests
  , fromTests
  , blockTests
  , declTests
  ]

declTests :: TestTree
declTests = testGroup "Variable Declaration Tests"
  [ testDec "Empty" (v [] [] [])
  , testDec "Only inp" (v ["x", "y"] [] [])
  , testDec "Only out" (v [] ["x", "y"] [])
  , testDec "Only tmp" (v [] [] ["x", "y"])
  , testDec "In inp and out" (v ["x", "y"] ["x", "y"] [])
  , testDec "No common" (v ["x"] ["y"] ["z"])
  , testDec "Complex" (v ["x", "y"] ["y", "z"] ["w"])
  , testDecN "Complex Wrong" (v ["x", "u", "y"] ["y", "z"] ["w", "u"])
  , testDecN "In tmp and inp" (v ["x"] [] ["x"])
  , testDecN "In tmp and out" (v [] ["x"] ["x"])
  , testDecN "Repeated inp" (v ["x", "x"] [] [])
  , testDecN "Repeated out" (v [] ["x", "x"] [])
  , testDecN "Repeated tmp" (v [] [] ["x", "x"])
  ]
  where
    v = VariableDecl
    testDec = wellformed wellformedDecl
    testDecN = malformed wellformedDecl

blockTests :: TestTree
blockTests = testGroup "Block Tests"
  [ testBlock "Wellformed" $
      Block ("x", ()) (From ("c", ())) [Skip] (Goto ("c", ()))
  , testBlockN "Goto entry" $
      Block ("x", ()) (From ("c", ())) [] (Goto ("t", ()))
  , testBlockN "From exit" $
      Block ("x", ()) (From ("t", ())) [] (Goto ("c", ()))
  , testBlockN "Mallformed body" $
      Block ("x", ()) (From ("t", ())) [Assert (Var "a")] (Goto ("c", ()))
  , testBlockN "Wrong jump" $
      Block ("y", ()) (From ("c", ())) [] (Goto ("c", ()))
  , testBlockN "Wrong from" $
      Block ("z", ()) (From ("c", ())) [] (Goto ("c", ()))
  , testBlockN "Nonexistant 1" $
      Block ("x", ()) (From ("a", ())) [] (Goto ("c", ()))
  , testBlockN "Nonexistant 2" $
      Block ("x", ()) (From ("c", ())) [] (Goto ("a", ()))
  ]
  where
    terminal = Block ("t", ()) (Entry ()) [] (Exit ())
    connective = Block ("c", ())
                       (Fi (Var "a") ("x", ()) ("y", ()))
                       []
                       (If (Var "a") ("x", ()) ("z", ()))
    prog = [terminal, connective]
    ns = ["x", "y", "z"]
    testBlock = wellformed $ wellformedBlock prog ns
    testBlockN = malformed $ wellformedBlock prog ns

fromTests :: TestTree
fromTests = testGroup "From Tests"
  [ testStep "Entry" (Entry ())
  , testStep "From" (From ("l", ()))
  , testStep "Fi" (Fi (Var "x") ("l1", ()) ("l2", ()))
  , testStepN "N. Fi" (Fi (Var "a") ("l1", ()) ("l2", ()))
  ]
  where
    testStep = wellformed $ wellformedFrom ["x", "y", "z"]
    testStepN = malformed $ wellformedFrom ["x", "y", "z"]

jumpTests :: TestTree
jumpTests = testGroup "Jump Tests"
  [ testStep "Exit" (Exit ())
  , testStep "Goto" (Goto ("l", ()))
  , testStep "If" (If (Var "x") ("l1", ()) ("l2", ()))
  , testStepN "N. If" (If (Var "a") ("l1", ()) ("l2", ()))
  ]
  where
    testStep = wellformed $ wellformedJump ["x", "y", "z"]
    testStepN = malformed $ wellformedJump ["x", "y", "z"]

stepTests :: TestTree
stepTests = testGroup "Step Tests"
  [ testStep "Skip" Skip
  , testStep "Assert" (Assert (Var "x"))
  , testStepN "N. Assert" (Assert (Var "a"))
  , testStep "Replacement" (Replacement (QVar "x") (QVar "y"))
  , testStepN "N. Replacement 1" (Replacement (QVar "a") (QVar "y"))
  , testStepN "N. Replacement 2" (Replacement (QVar "x") (QVar "a"))
  , testStep "Update" (Update "x" Add (Var "y"))
  , testStepN "N. Update" (Update "x" Add (Var "a"))
  , testStepN "LHS Var in RHS" (Update "x" Add (Var "x"))
  ]
  where
    testStep = wellformed $ wellformedStep ["x", "y", "z"]
    testStepN = malformed $ wellformedStep ["x", "y", "z"]

patTests :: TestTree
patTests = testGroup "Pattern Tests"
  [ testPat "Constant" (QConst $ Num 3)
  , testPat "Variable" (QVar "x")
  , testPat "Pair" (QPair (QVar "y") (QVar "x"))
  , testPat "Big" (QPair (QPair (QVar "y") (QConst $ Num 1))
                         (QPair (QVar "z") (QVar "x")))
  , testPatN "N. Variable" (QVar "a")
  , testPatN "N. Pair 1" (QPair (QVar "y") (QVar "a"))
  , testPatN "N. Pair 2" (QPair (QVar "b") (QVar "x"))
  , testPatN "Linearity" (QPair (QVar "x") (QVar "x"))
  ]
  where
    testPat = wellformed $ wellformedPat ["x", "y", "z"]
    testPatN = malformed $ wellformedPat ["x", "y", "z"]

exprTests :: TestTree
exprTests = testGroup "Expression Tests"
  [ testExp "Constant" (Const $ Num 3)
  , testExp "Variable" (Var "y")
  , testExp "Unary Op" (UOp Not $ Var "y")
  , testExp "Binary Op" (Op Mul (Var "x") (Var "z"))
  , testExpN "N. Variable" (Var "a")
  , testExpN "N. Unary" (UOp Not $ Var "a")
  , testExpN "N. Binary 1" (Op Mul (Var "x") (Var "a"))
  , testExpN "N. Binary 2" (Op Mul (Var "a") (Var "z"))
  ]
  where
    testExp = wellformed $ wellformedExp ["x", "y", "z"]
    testExpN = malformed $ wellformedExp ["x", "y", "z"]
