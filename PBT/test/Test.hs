import Test.Tasty

import qualified Test.Sort
import qualified Test.Unit
import qualified Test.Compiler

main :: IO ()
main = do
  defaultMain (testGroup "All Tests"
                [ testGroup "Sort" Test.Sort.props
                , testGroup "Compiler" Test.Compiler.props
                , testGroup "Unit" Test.Unit.unitTests               
                ])