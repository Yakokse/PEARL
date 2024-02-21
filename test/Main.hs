import Test.Tasty

import qualified ParserTests as P
import qualified InversionTests as I
import qualified WellformedTests as W

main :: IO ()
main = defaultMain $ 
  testGroup "All tests" 
    [ P.tests
    , I.tests
    , W.tests
    ]
