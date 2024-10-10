import qualified Data.Map.Strict as M
import Lib (Expr (..), eval)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Data.Either (isLeft)

testEval :: TestTree
testEval =
  testGroup "Eval" [testAdd, testVar]
  where
    testEvalNoVarSuccess msg expr res =
      testCase msg $ eval M.empty expr @?= Right res
    testAdd =
      testGroup
        "Add"
        [ testCase "1 + 2 == 3" $ eval M.empty (Add (Lit 1) (Lit 2)) @?= Right 3,
          testCase "2 + 1 == 3" $ eval M.empty (Add (Lit 2) (Lit 1)) @?= Right 3,
          testCase "2 + 1 == 3 as Lit instance" $ eval M.empty (2 + 1) @?= Right 3,
          testEvalNoVarSuccess "0+2 == 2" (0 + 2) 2
        ]
    testVar =
      testGroup
        "Var"
        [ testSuccess "x + 1 == 3, x == 2" (M.singleton "x" 2) (Add (Var "x") (Lit 1)) 3,
          testFailure "x + 1 fails when x isn't assigned" M.empty (Add (Var "x") (Lit 1)),
          testSuccess "x + 1 == 3, x == 2" (M.singleton "x" 1) (Add (Var "x") (Lit 1)) 2
        ]
    testSuccess msg state expr expected =
      testCase msg $
        case eval state expr of
          Right x -> assertBool "Evaluation result is wrong" (x == expected)
          Left _ -> assertFailure "Expression failed to evaluate"
    testFailure msg state expr =
      testCase msg $
        assertBool "Expr should fail to evaluate in the state" $
          isLeft $
            eval state expr

main :: IO ()
main =
  defaultMain $ testGroup "Expressions" [testEval]