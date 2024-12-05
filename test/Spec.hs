import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

import Interpreter

main :: IO ()
main = defaultMain $ testGroup "all-tests" tests

tests :: [TestTree]
tests =
  [ testGroup "Property" propTests
  , testGroup "Unit" unitTests
  ]

propTests :: [TestTree]
propTests = []

unitTests :: [TestTree]
unitTests = []
