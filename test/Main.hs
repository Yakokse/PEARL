import Test.Tasty

import qualified ParserTests as P

main :: IO ()
main = defaultMain $ 
  testGroup "All tests" 
    [
      P.tests
    ]