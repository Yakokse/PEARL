module WellformedTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Utils.Error

import RL.Values
import RL.AST

import RL.Impl.Wellformed


wellformed :: (a -> EM b) -> TestName -> a -> TestTree
wellformed f n x = testCase n $ case f x of Right _ -> return (); Left e -> assertFailure e

malformed :: (a -> EM b) -> TestName -> a -> TestTree
malformed f n x = testCase n $ case f x of Left _ -> return (); _ -> assertFailure "Unexpectedly wellformed"

tests :: TestTree
tests = testGroup "All Inversion Tests"
  [ patTests
  , stepTests
  , jumpTests
  , fromTests
  , blockTests
  ]

blockTests :: TestTree
blockTests = testGroup "Block Tests"
  []

fromTests :: TestTree
fromTests = testGroup "From Tests"
  []

jumpTests :: TestTree
jumpTests = testGroup "Jump Tests"
  []

stepTests :: TestTree
stepTests = testGroup "Step Tests"
  []

patTests :: TestTree
patTests = testGroup "Pattern Tests"
  []
