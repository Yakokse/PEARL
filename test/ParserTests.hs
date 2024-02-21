module ParserTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Impl.Parser
import AST
import Values


testPos :: (Eq a, Show a) => Parser a -> TestName -> String -> a -> TestTree
testPos p n x y = testCase n $ parseStr p x @?= Right y

testNeg :: Show a => Parser a -> TestName -> String -> TestTree
testNeg p n x = testCase n $ 
                      case parseStr p x of
                        Left _ -> return ()
                        Right e -> assertFailure $ "Expression unexpectedly parsed: " ++ show e

tests :: TestTree
tests = testGroup "All parsing tests" 
  [ expTests
  , patTests
  ]

patTests :: TestTree
patTests = testGroup "Pattern Tests"
  [ testPat "Var" "x" (QVar "x")
  , testPat "Number" "'1" (QConst $ Num 1)
  , testPat "Atom" "'atom" (QConst $ Atom "atom")
  , testPat "Pair" "(x . y)" (QPair (QVar "x") (QVar "y"))
  , testPat "Const Pair" "'(1 . 2)" (QConst $ Pair (Num 1) (Num 2))
  , testPat "Complex" "((x . 'nil) . ('(1 . 2) . y))" $
      QPair (QPair (QVar "x") (QConst Nil)) 
            (QPair (QConst $ Pair (Num 1) (Num 2)) (QVar "y"))
  , testPatN "Extra parens" "(x)" 
  , testPatN "Missing parens" "x . y" 
  , testPatN "Mismatched 1" "(x" 
  , testPatN "Mismatched 2" "x)" 
  ]
  where 
    testPat  = testPos pPattern
    testPatN = testNeg pPattern

expTests :: TestTree
expTests = testGroup "Expression Tests"
  [ testExp "Number" "'1" (Const $ Num 1)
  , testExp "Nil" "'nil" (Const Nil)
  , testExp "Atom" "'atom" (Const $ Atom "atom")
  , testExp "Var" "x" (Var "x")
  , testExp "Const pair" "'(1 . 2)" (Const $ Pair (Num 1) (Num 2))
  , testExp "Pair of const" "('1 . '2)" (Op Cons (Const $ Num 1) (Const $ Num 2))
  , testExp "Binary operator 1" "x + y" (Op (ROp Add) (Var "x") (Var "y"))
  , testExp "Binary operator 2" "x < y" (Op Less (Var "x") (Var "y"))
  , testExp "Binary operator 3" "x * y" (Op Mul (Var "x") (Var "y"))
  , testExp "Unary operator 1" "hd x" (UOp Hd (Var "x") )
  , testExp "Unary operator 2" "tl x" (UOp Tl (Var "x") )
  , testExp "Unary operator 3" "!x" (UOp Not (Var "x") )
  , testExp "Repeated brackets 1" "(((((((((((((((((x)))))))))))))))))" (Var "x")
  , testExp "Repeated brackets 2" "(((((((((((((((((x . y)))))))))))))))))" 
            (Op Cons (Var "x") (Var "y"))
  , testExp "Repeated brackets 3" "(((((((((((((((((x)))))).(((((((y))))))))))))))))))" 
            (Op Cons (Var "x") (Var "y"))
  , testExp "Repeated unary op" "!(!(!x))" (UOp Not(UOp Not(UOp Not (Var "x"))))
  , testExp "Associativity 1" "x - y - z" 
      (Op (ROp Sub) (Op (ROp Sub) (Var "x") (Var "y")) (Var "z"))
  , testExp "Associativity 2" "x / y / z" (Op Div (Op Div (Var "x") (Var "y")) (Var "z"))
  , testExp "Same Precedence 1" "a + b - c" 
      (Op (ROp Sub) (Op (ROp Add) (Var "a") (Var "b")) (Var "c"))
  , testExp "Same Precedence 2" "a - b + c" 
      (Op (ROp Add) (Op (ROp Sub) (Var "a") (Var "b")) (Var "c"))
  , testExp "Same Precedence 3" "a * b / c" 
      (Op Div (Op Mul (Var "a") (Var "b")) (Var "c"))
  , testExp "Same Precedence 4" "a / b * c" 
      (Op Mul (Op Div (Var "a") (Var "b")) (Var "c"))
  , testExp "Big Precedence 1" "a * b + c < d && e . f" $
    Op Cons (Op And (Op Less (Op (ROp Add) (Op Mul 
      (Var "a") (Var "b")) (Var "c")) (Var "d")) (Var "e")) (Var "f")
  , testExp "Big Precedence 2" "a . b && c < d + e * f" $
    Op Cons (Var "a") (Op And (Var "b") (Op Less (Var "c") 
      (Op (ROp Add) (Var "d") (Op Mul (Var "e") (Var "f")))))
  , testExpN "Missing \"'\" 1" "nil"
  , testExpN "Missing \"'\" 2" "1"
  , testExpN "Missing \"'\" 3" "(1 . 2)"
  , testExpN "Mallformed exp 1" "x + "
  , testExpN "Mallformed exp 2" " / y"
  , testExpN "Mallformed exp 3" "hd"
  , testExpN "Mismatched parens 1" "(((x))"
  , testExpN "Mismatched parens 2" "((x)))"
  ]
  where 
    testExp  = testPos pExpr
    testExpN = testNeg pExpr
    