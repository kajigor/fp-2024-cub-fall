
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Tests.Dictionary
import Tests.Spellcheck
import Tests.Utils

main :: IO ()
main =
  defaultMain $ testGroup "Tests" [testDictionary, testSpellcheck, testUtils]
