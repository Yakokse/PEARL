import Test.Tasty

import qualified ParserTests as PS
import qualified InversionTests as INV
import qualified WellformedTests as WF
import qualified OperatorTests as OP
import qualified UniformTests as UNI
import qualified PointwiseTests as PW
import qualified AnnotateTests as AN

main :: IO ()
main = defaultMain $ 
  testGroup "All tests" 
    [ PS.tests
    , INV.tests
    , WF.tests
    , OP.tests
    , UNI.tests
    , PW.tests
    , AN.tests
    ]
