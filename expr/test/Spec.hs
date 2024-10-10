import qualified Data.Map.Strict as M
import Data.Either (isLeft)
import Expr (Expr (..), eval)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import Expr
import BaseExpr

testEval :: TestTree
testEval =
  testGroup "Eval" [testVar, testLet, testUnary, testPlus, testMinus, testMult, testDiv, testPow]
  where
    testEvalNoVarSuccess msg expr res =
      testCase msg $ eval M.empty expr @?= Right res
    testVar =
      testGroup
        "Var"
        [ testSuccess "x + 1 == 3, x == 2" (M.singleton "x" (Number 2)) (Plus (Var "x") (Number 1)) 3,
          testFailure "x + 1 fails when x isn't assigned" M.empty (Plus (Var "x") (Number 1)),
          testSuccess "x + 1 == 3, x == 2" (M.singleton "x" (Number 1)) (Plus (Var "x") (Number 1)) 2
        ]
    testLet =
      testGroup
        "Let"
        [
          testSuccess "x := 1, x + x == 2" M.empty (Let "x" (Number 1) $ Plus (Var "x") (Var "x")) 2,
          testSuccess "x := 1, y := 5,  x + y == 6" M.empty (Let "x" (Number 1) $ Let "y" (Number 5) $ Plus (Var "x") (Var "y")) 6,
          testSuccess "lazy evluation" M.empty (Let "y" (Var "x") $ Let "x" (Number 1) $ Plus (Var "x") (Number 1)) 2,
          testSuccess "reassignment" (M.singleton "x" (Number 1)) (Let "x" (Number 2) $ Var "x") 2
        ]
    testUnary =
      testGroup
        "Unary"
        [
          testSuccess "sqrt(x), x == 9" (M.singleton "x" (Number 9)) (Sqrt $ Var "x") 3,
          testFailure "sqrt(x), x == -1 fails" (M.singleton "x" (Number $ -1)) (Sqrt $ Var "x"),
          testSuccess "abs(x), x == 11" (M.singleton "x" (Number 11)) (Abs $ Var "x") 11,
          testSuccess "abs(x), x == -13" (M.singleton "x" (Number $ -13)) (Abs $ Var "x") 13,
          testSuccess "signum(x), x == 100" (M.singleton "x" (Number 100)) (Sgn $ Var "x") 1,
          testSuccess "signum(x), x == -199" (M.singleton "x" (Number $ -100)) (Sgn $ Var "x") (-1),
          testSuccess "signum(x), x == 0" (M.singleton "x" (Number 0)) (Sgn $ Var "x") 0,
          testSuccess "-x, x == 1" (M.singleton "x" (Number 1)) (UnaryMinus $ Var "x") (-1),
          testSuccess "-x, x == -1" (M.singleton "x" (Number $ -1)) (UnaryMinus $ Var "x") 1
        ]
    testPlus =
      testGroup
        "Plus"
        [ testCase "1 + 2 == 3" $ eval M.empty (Plus (Number 1) (Number 2)) @?= Right 3,
          testCase "2 + 1 == 3" $ eval M.empty (Plus (Number 2) (Number 1)) @?= Right 3,
          testCase "2 + 1 == 3 as Number instance" $ eval M.empty (Number $ 2 + 1) @?= Right 3,
          testEvalNoVarSuccess "0+2 == 2" (Number $ 0 + 2) 2
        ]
    testMinus =
      testGroup
        "Minus"
        [ testCase "3 - 2 == 1" $ eval M.empty (Minus (Number 3) (Number 2)) @?= Right 1,
          testCase "(-2) - (-3) == 1" $ eval M.empty (Minus (Number $ -2) (Number $ -3)) @?= Right 1,
          testCase "2 - 1 == 1 as Number instance" $ eval M.empty (Number $ 2 - 1) @?= Right 1,
          testEvalNoVarSuccess "0-2 == 2" (Number $ 0 - 2) (-2)
        ]
    testMult =
      testGroup
        "Mult"
        [ testCase "2 * 5 == 10" $ eval M.empty (Mult (Number 2) (Number 5)) @?= Right 10,
          testCase "5 * 2 == 10" $ eval M.empty (Mult (Number 5) (Number 2)) @?= Right 10,
          testCase "2 * 2 == 4 as Number instance" $ eval M.empty (Number $ 2 * 2) @?= Right 4,
          testEvalNoVarSuccess "0*2 == 0" (Number $ 0 * 2) 0
        ]
    testDiv =
      testGroup
        "Div"
        [ testCase "1 / 2 == 0.5" $ eval M.empty (Div (Number 1) (Number 2)) @?= Right 0.5,
          testCase "0 / 10 == 0" $ eval M.empty (Div (Number 0) (Number 10)) @?= Right 0,
          testCase "1 / 2 == 0.5 as Number instance" $ eval M.empty (Number $ 1 / 2) @?= Right 0.5,
          testFailure "1 / 0 fails" M.empty (Div (Number 1) (Number 0)),
          testFailure "0 / 0 fails" M.empty (Div (Number 0) (Number 0))

        ]
    testPow =
      testGroup
        "Pow"
        [ testCase "666 ^ 0 == 1" $ eval M.empty (Pow (Number 666) (Number 0)) @?= Right 1,
          testCase "2 ^ 10 == 1024" $ eval M.empty (Pow (Number 2) (Number 10)) @?= Right 1024,
          testCase "2 + 1 == 3 as Number instance" $ eval M.empty (Number $ 2 + 1) @?= Right 3,
          testEvalNoVarSuccess "0+2 == 2" (Number $ 0 + 2) 2,
          testFailure "0^0 fails"  M.empty (Pow (Number 0) (Number 0)),
          testFailure "0^(-1) fails" M.empty (Pow (Number 0) (Number (-1))),
          testCase "0^(0.5) == 0" $ eval M.empty (Pow (Number 0) (Number 0.5)) @?= Right 0,
          testFailure "(-1)^0.5 fails" M.empty (Pow (Number (-1)) (Number 0.5)),
          testFailure "(-1)^(-1) fails" M.empty (Pow (Number (-1)) (Number (-1)))
        ]
    testSuccess msg state expr expected =
      testCase msg $
        case eval state expr of
          Right x -> assertBool "Evaluation result is wrong" (x == expected)
          _ -> assertFailure "Expression failed to evaluate"
    testFailure msg state expr =
      testCase msg $
        assertBool "Expr should fail to evaluate in the state" $
          isLeft $
            eval state expr

main :: IO ()
main =
  defaultMain $ testGroup "Expressions" [testEval]