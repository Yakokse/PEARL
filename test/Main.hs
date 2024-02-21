import Test.Tasty

import qualified ParserTests as P
import qualified InversionTests as I
import qualified WellformedTests as W
import qualified OperatorTests as O
main :: IO ()
main = defaultMain $ 
  testGroup "All tests" 
    [ P.tests
    , I.tests
    , W.tests
    , O.tests
    ]
