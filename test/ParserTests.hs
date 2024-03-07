module ParserTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import RL.Values
import RL.AST

import Parsing.Impl.Common
import Parsing.Impl.Parser


testPos :: (Eq a, Show a) => Parser a -> TestName -> String -> a -> TestTree
testPos p n x y = testCase n $ parseStr p x @?= Right y

testNeg :: Show a => Parser a -> TestName -> String -> TestTree
testNeg p n x = testCase n $
                      case parseStr p x of
                        Left _ -> return ()
                        Right e -> assertFailure $ "Expression unexpectedly parsed: " ++ show e

-- todo: roundtrip tests?
tests :: TestTree
tests = testGroup "All Parsing Tests"
  [ expTests
  , patTests
  , stepTests
  , jumpTests
  , fromTests
  , blockTests
  , declTests
  , progTests
  ]

progTests :: TestTree
progTests = testGroup "Program Tests"
  [ testProg "Smallest" (emptyDeclStr ++ block1Str)
      (emptyDecl, [block1])
  , testProg "Medium" (concat [emptyDeclStr, block1Str, block2Str])
      (emptyDecl, [block1, block2])
  , testProg "Big" (concat [normDeclStr, block1Str, block2Str, block3Str])
      (normDecl, [block1, block2, block3])
  , testProgN "Empty" ""
  , testProgN "Decl missing" block1Str
  , testProgN "Block missing" normDeclStr
  , testProgN "Wrong order" $ block1Str ++ normDeclStr
  ]
  where
    emptyDeclStr = "() -> () "
    emptyDecl  = VariableDecl [] [] []
    normDeclStr  = "(a) -> (b) with (c) "
    normDecl   = VariableDecl ["a"] ["b"] ["c"]
    block1Str = "l: entry exit "
    block1 = Block ("l", ()) (Entry ()) [] (Exit ())
    block2Str = "l1: from l2 skip skip goto l3 "
    block2 = Block ("l1", ())
            (From ("l2", ()))
            [Skip, Skip]
            (Goto ("l3", ()))
    block3Str = "l1: fi a from l2 else l3 x <- y z += '3 if b goto l4 else l5"
    block3 = Block ("l1", ())
            (Fi (Var "a") ("l2", ()) ("l3", ()))
            [ Replacement (QVar "x") (QVar "y")
            , Update "z" Add (Const $ Num 3)]
            (If (Var "b") ("l4", ()) ("l5", ()))
    testProg  = testPos pProg
    testProgN = testNeg pProg

declTests :: TestTree
declTests = testGroup "Variable Declaration Tests"
  [ testDecl "Empty 1" "() -> ()" $ VariableDecl [] [] []
  , testDecl "Empty 2" "() -> () with ()" $ VariableDecl [] [] []
  , testDecl "Simple 1" "(a) -> (b)" $ VariableDecl ["a"] ["b"] []
  , testDecl "Simple 2" "(a) -> (b) with (c)" $
      VariableDecl ["a"] ["b"] ["c"]
  , testDecl "Big 1" "(a b c d) -> (e f g h)" $
      VariableDecl ["a", "b", "c", "d"] ["e", "f", "g", "h"] []
  , testDecl "Big 2" "(a b c d) -> (e f g h) with (i j k)" $
      VariableDecl ["a", "b", "c", "d"] ["e", "f", "g", "h"] ["i", "j", "k"]
  , testDeclN "Missing input" "-> ()"
  , testDeclN "Missing output" "() ->"
  , testDeclN "Empty" ""
  , testDeclN "Wrong order" "() with () -> ()"
  ]
  where
    testDecl  = testPos pDecl
    testDeclN = testNeg pDecl

blockTests :: TestTree
blockTests = testGroup "Block Tests"
  [ testBlock "Empty" "l: entry exit" $
      Block ("l", ()) (Entry ()) [] (Exit ())
  , testBlock "Basic 1" "l: entry skip exit" $
      Block ("l", ()) (Entry ()) [Skip] (Exit ())
  , testBlock "Basic 2" "l1: from l2 skip skip goto l3" $
      Block ("l1", ()) (From ("l2", ()))
                       [Skip, Skip]
                       (Goto ("l3", ()))
  , testBlock "Complex" "l1: fi a from l2 else l3 x <- y z += '3 if b goto l4 else l5" $
     Block ("l1", ()) (Fi (Var "a") ("l2", ()) ("l3", ()))
                       [ Replacement (QVar "x") (QVar "y")
                       , Update "z" Add (Const $ Num 3)]
                       (If (Var "b") ("l4", ()) ("l5", ()))
  , testBlockN "Missing \":\"" "l entry skip exit"
  , testBlockN "Missing from" "l: skip exit"
  , testBlockN "Missing jump" "l: entry skip"
  ]
  where
    testBlock  = testPos pBlock
    testBlockN = testNeg pBlock

fromTests :: TestTree
fromTests = testGroup "Come-from Tests"
  [ testFrom "Entry" "entry" $ Entry ()
  , testFrom "From" "from l" $ From ("l", ())
  , testFrom "Fi 1" "fi e from l1 else l2" $
      Fi (Var "e") ("l1", ()) ("l2", ())
  , testFrom "Fi 2" "fi x * y from l1 else l2" $
      Fi (Op Mul (Var "x") (Var "y")) ("l1", ()) ("l2", ())
  , testFromN "Case Sensitive 1" "Entry"
  , testFromN "Case Sensitive 2" "From l"
  , testFromN "Case Sensitive 3" "Fi e from l1 else l2"
  , testFromN "Forbidden label 1" "from entry"
  , testFromN "Forbidden label 2" "from exit"
  , testFromN "Forbidden label 3" "fi e from entry else l"
  , testFromN "Forbidden label 4" "fi e from l else exit"
  ]
  where
    testFrom  = testPos pFrom
    testFromN = testNeg pFrom

jumpTests :: TestTree
jumpTests = testGroup "Jump Tests"
  [ testJump "Exit" "exit" $ Exit ()
  , testJump "Goto" "goto l" $ Goto ("l", ())
  , testJump "If 1" "if e goto l1 else l2" $
      If (Var "e") ("l1", ()) ("l2", ())
  , testJump "If 2" "if x * y goto l1 else l2" $
      If (Op Mul (Var "x") (Var "y")) ("l1", ()) ("l2", ())
  , testJumpN "Case Sensitive 1" "Exit"
  , testJumpN "Case Sensitive 2" "Goto l"
  , testJumpN "Case Sensitive 3" "If e goto l1 else l2"
  , testJumpN "Forbidden label 1" "goto entry"
  , testJumpN "Forbidden label 2" "goto exit"
  , testJumpN "Forbidden label 3" "if e goto entry else l"
  , testJumpN "Forbidden label 4" "if e goto l else exit"
  ]
  where
    testJump  = testPos pJump
    testJumpN = testNeg pJump

stepTests :: TestTree
stepTests = testGroup "Step Tests"
  [ testStep "Skip" "skip" Skip
  , testStep "Assert 1" "assert(x)" $ Assert (Var "x")
  , testStep "Assert 2" "assert(x*y)" $
      Assert (Op Mul (Var "x") (Var "y"))
  , testStep "Replacement 1" "x <- y" $
      Replacement (QVar "x") (QVar "y")
  , testStep "Replacement 2" "(x . y) <- (y . x)" $
      Replacement (QPair (QVar "x") (QVar "y")) (QPair (QVar "y") (QVar "x"))
  , testStep "Update 1" "x += y" $ Update "x" Add (Var "y")
  , testStep "Update 2" "x -= y" $ Update "x" Sub (Var "y")
  , testStep "Update 3" "x ^= y" $ Update "x" Xor (Var "y")
  , testStep "Update 4" "x += y * z" $
      Update "x" Add (Op Mul (Var "y") (Var "z"))
  , testStepN "Empty" ""
  , testStepN "Case Sensitive 1" "Skip"
  , testStepN "Case Sensitive 2" "Assert(x)"
  , testStepN "Malformed Assert 1" "assert()"
  , testStepN "Malformed Assert 2" "assert x"
  , testStepN "Malformed Replacement 1" "x <-"
  , testStepN "Malformed Replacement 2" " <- y"
  , testStepN "Malformed Update 1" "x += "
  , testStepN "Malformed Update 2" " += x"
  , testStepN "Non-rev operator 1" "x *= "
  , testStepN "Non-rev operator 2" "x /= "
  ]
  where
    testStep  = testPos pStep
    testStepN = testNeg pStep

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
  , testPatN "Missing \"'\" 1" "1"
  , testPatN "Missing \"'\" 2" "nil"
  , testPatN "Missing \"'\" 3" "(1 . 2)"
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
  , testExp "Repeated brackets 1"
    "(((((((((((((((((((((((((((((x)))))))))))))))))))))))))))))" (Var "x")
  , testExp "Repeated brackets 2"
    "(((((((((((((((((((((((((((((x . y)))))))))))))))))))))))))))))"
            (Op Cons (Var "x") (Var "y"))
  , testExp "Repeated brackets 3"
    "((((((((((((((((((((((((((((((((((((((x))))))))))))))))))))))))))).(((((((((y))))))))))))))))))))"
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
