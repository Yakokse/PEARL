module OperatorTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Operators
import AST
import Values

tests :: TestTree
tests = testGroup "All Operator Tests"
  [ unaryTests
  , revOpTests
  , binOpTests
  ]

binOpTests :: TestTree
binOpTests = testGroup "Bin. Op Tests"
  [ test "Times" Mul (Num 3) (Num 5) (Num 15)
  , test "Divide 1" Div (Num 15) (Num 5) (Num 3)
  , test "Divide 2" Div (Num 3) (Num 2) (Num 1)
  , testTrue "And True" And (Atom "True") (Num 4)
  , test "And False 1" And Nil (Num 4) Nil
  , test "And False 2" And (Atom "True") Nil Nil
  , test "And False 3" And Nil Nil Nil
  , testTrue "Or True 1" Or (Atom "True") Nil
  , testTrue "Or True 2" Or Nil (Num 4)
  , testTrue "Or True 2" Or (Atom "True") (Num 4)
  , test "Or False" Or Nil Nil Nil
  , testTrue "Less True" Less (Num 3) (Num 5)
  , test "Less False" Less (Num 6) (Num 5) Nil
  , testTrue "Greater True" Greater (Num 7) (Num 5)
  , test "Greater False" Greater (Num 2) (Num 5) Nil
  , test "Cons" Cons (Atom "a") (Atom "b") (Pair (Atom "a") (Atom "b"))
  , testTrue "Equal True 1" Equal (Num 3) (Num 3)
  , testTrue "Equal True 2" Equal Nil Nil
  , testTrue "Equal True 3" Equal (Atom "a") (Atom "a")
  , test "Equal False 1" Equal (Num 3) (Num 4) Nil
  , test "Equal False 2" Equal (Atom "a") (Atom "b") Nil
  , test "Equal False 3" Equal Nil (Pair Nil Nil) Nil
  , testN "Division by 0" Div (Num 3) (Num 0)
  ]
  where
    testTrue n o x y =
      testCase n $ case calc o x y of
                    Right v | truthy v -> return ()
                    _ -> assertFailure "False false"
    test n o x y z =
      testCase n $ case calc o x y of
                    Right res -> res @?= z
                    Left e -> assertFailure e
    testN n o x y  =
      testCase n $ case calc o x y of
                    Left _ -> return ()
                    _ -> assertFailure "Error expected"

revOpTests :: TestTree
revOpTests = testGroup "Rev. Op Tests"
  [ test "Addition 1" Add (Num 1) (Num 2) (Num 3)
  , test "Addition 2" Add (Num 3) (Num 7) (Num 10)
  , test "Subtraction 1" Sub (Num 6) (Num 3) (Num 3)
  , test "Subtraction 2" Sub (Num 3) (Num 2) (Num 1)
  , test "XOR 1" Xor (Num 3) (Num 3) Nil
  , test "XOR 2" Xor (Num 3) Nil (Num 3)
  , test "XOR 3" Xor Nil (Num 3) (Num 3)
  , test "XOR 4" Xor Nil Nil Nil
  , test "XOR 5" Xor (Atom "a") (Atom "a") Nil
  , test "XOR 6" Xor (Pair (Num 1) (Atom "a")) (Pair (Num 1) (Atom "a")) Nil
  , testN "N. Addition 1" Add Nil (Num 2)
  , testN "N. Addition 2" Add (Atom "a") (Num 2)
  , testN "N. Addition 3" Add (Pair Nil Nil) (Num 2)
  , testN "N. Subtraction 1" Sub Nil (Num 2)
  , testN "N. Subtraction 2" Sub (Atom "a") (Num 2)
  , testN "N. Subtraction 3" Sub (Pair Nil Nil) (Num 2)
  , testN "N. XOR 1" Xor (Atom "a") (Num 2)
  , testN "N. XOR 2" Xor (Num 3) (Num 2)
  , testN "N. XOR 3" Xor (Atom "a") (Atom "b")
  ]
  where
    test n o x y z =
      testCase n $ case calcR o x y of Right res -> res @?= z; Left e -> assertFailure e
    testN n o x y  =
      testCase n $ case calcR o x y of Left _ -> return (); _ -> assertFailure "Error expected"

unaryTests :: TestTree
unaryTests = testGroup "Unary Tests"
  [ test "Head" Hd (Pair (Num 1) (Num 2)) (Num 1)
  , test "Tail" Tl (Pair (Num 1) (Num 2)) (Num 2)
  , test "Not 1" Not (Atom "a") Nil
  , testCase "Not 2" test'
  , testN "N. Head" Hd (Num 1)
  , testN "N. Tail" Tl (Num 1)
  ]
  where
    test' = case calcU Not Nil of Right v | truthy v -> return (); _ -> assertFailure "False false"
    test n o x y =
      testCase n $ case calcU o x of Right res -> res @?= y; Left e -> assertFailure e
    testN n o x =
      testCase n $ case calcU o x of Left _ -> return (); _ -> assertFailure "Error expected"
