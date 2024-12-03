module InversionTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import RL.AST

import Inversion.Impl.Inverter

testEq :: (Eq b, Show b) => (a -> b) -> TestName -> a -> b -> TestTree
testEq f n x y = testCase n $ f x @?= y

tests :: TestTree
tests = testGroup "All Inversion Tests"
  [ stepTests
  , jumpTests
  , fromTests
  , blockTests
  ]


blockTests :: TestTree
blockTests = testGroup "Block Tests"
  [ testInv "Block"
     (Block ("l", ()) (Entry (QVar "a") ()) [Skip, Update "x" Add (Var "y")] (Goto ("l1", ())))
     (Block ("l", ()) (From ("l1", ())) [Update "x" Sub (Var "y"), Skip] (Exit (QVar "a") ()))
  ]
  where
    testInv = testEq invertBlock

fromTests :: TestTree
fromTests = testGroup "Come-from Tests"
  [ testInv "Exit" (Entry (QVar "a") ()) (Exit (QVar "a") ())
  , testInv "Goto" (From ("l", ())) (Goto ("l", ()))
  , testInv "If"
        (Fi (Var "e") ("l1", ()) ("l2", ()))
        (If (Var "e") ("l1", ()) ("l2", ()))
  ]
  where
    testInv = testEq invertFrom

jumpTests :: TestTree
jumpTests = testGroup "Jump Tests"
  [ testInv "Exit" (Exit (QVar "a") ()) (Entry (QVar "a") ())
  , testInv "Goto" (Goto ("l", ())) (From ("l", ()))
  , testInv "If"
        (If (Var "e") ("l1", ()) ("l2", ()))
        (Fi (Var "e") ("l1", ()) ("l2", ()))
  ]
  where
    testInv = testEq invertJump

stepTests :: TestTree
stepTests = testGroup "Step Tests"
  [ testInv "Skip" Skip Skip
  , testInv "Assert" (Assert (Var "x")) (Assert (Var "x"))
  , testInv "Replacement"
        (Replacement (QVar "a") (QVar "b"))
        (Replacement (QVar "b") (QVar "a"))
  , testInv "Update +"
        (Update "x" Add (Var "y"))
        (Update "x" Sub (Var "y"))
  , testInv "Update -"
        (Update "x" Sub (Var "y"))
        (Update "x" Add (Var "y"))
  , testInv "Update ^"
        (Update "x" Xor (Var "y"))
        (Update "x" Xor (Var "y"))
  ]
  where
    testInv = testEq invertStep
