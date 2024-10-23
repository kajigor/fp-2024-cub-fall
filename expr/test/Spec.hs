import qualified Data.Map.Strict as M
import Data.Either (isLeft)
import Expr (Expr(..))
import Interpreter (eval)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

testEval :: TestTree
testEval =
  testGroup "Eval" [testAdd, testSub, testMul, testDiv, testPow, testSqrt, testVar, testLet, testNestedExpr, testComplexExpr]
  where
    testAdd =
      testGroup
        "Add"
        [ testCase "1 + 2 == 3" $ eval M.empty (Add (Num 1) (Num 2)) @?= Right 3,
          testCase "2 + 1 == 3" $ eval M.empty (Add (Num 2) (Num 1)) @?= Right 3,
          testEvalNoVarSuccess "0 + 2 == 2" (Add (Num 0) (Num 2)) 2,
          testEvalNoVarSuccess "2 * 3 + 10 / 2 == 11" (Add (Mul (Num 2) (Num 3)) (Div (Num 10) (Num 2))) 11
        ]

    testSub =
      testGroup
        "Sub"
        [ testEvalNoVarSuccess "10 - 4 == 6" (Sub (Num 10) (Num 4)) 6,
          testEvalNoVarSuccess "20 - 10 == 10" (Sub (Num 20) (Num 10)) 10,
          testEvalNoVarSuccess "100 - 50 == 50" (Sub (Num 100) (Num 50)) 50,
          testFailure "0 - sqrt(-4) fails" M.empty (Sub (Num 0) (Sqrt (Num (-4))))
        ]

    testMul =
      testGroup
        "Mul"
        [ testEvalNoVarSuccess "3 * 4 == 12" (Mul (Num 3) (Num 4)) 12,
          testEvalNoVarSuccess "7 * 6 == 42" (Mul (Num 7) (Num 6)) 42,
          testEvalNoVarSuccess "0 * 100 == 0" (Mul (Num 0) (Num 100)) 0
        ]

    testDiv =
      testGroup
        "Div"
        [ testEvalNoVarSuccess "10 / 2 == 5" (Div (Num 10) (Num 2)) 5,
          testEvalNoVarSuccess "9 / 3 == 3" (Div (Num 9) (Num 3)) 3,
          testFailure "10 / 0 fails" M.empty (Div (Num 10) (Num 0)),
          testFailure "sqrt(16) / 0 fails" M.empty (Div (Pow (Num 16) (Num 0.5)) (Num 0))
        ]

    testPow =
      testGroup
        "Pow"
        [ testEvalNoVarSuccess "2 ^ 3 == 8" (Pow (Num 2) (Num 3)) 8,
          testEvalNoVarSuccess "9 ^ 0.5 == 3" (Pow (Num 9.0) (Num 0.5)) 3,
          testEvalNoVarSuccess "16^(1/4) == 2" (Pow (Num 16) (Num (1/4))) 2,
          testEvalNoVarSuccess "27^(1/3) == 3" (Pow (Num 27) (Num (1/3))) 3,
          testEvalNoVarSuccess "5 ^ 0 == 1" (Pow (Num 5) (Num 0)) 1
        ]

    testSqrt =
      testGroup
        "Sqrt"
        [ testEvalNoVarSuccess "sqrt(16) == 4" (Sqrt (Num 16)) 4,
          testEvalNoVarSuccess "sqrt(4) == 2" (Sqrt (Num 4)) 2,
          testFailure "sqrt(-9) fails" M.empty (Sqrt (Num (-9))),
          testFailure "sqrt(-16) fails" M.empty (Sqrt (Num (-16)))
        ]

    testVar =
      testGroup
        "Var"
        [ testSuccess "x + 1 == 3, x == 2" (M.singleton "x" 2) (Add (Var "x") (Num 1)) 3,
          testFailure "x + 1 fails when x isn't assigned" M.empty (Add (Var "x") (Num 1)),
          testSuccess "x + 1 == 2, x == 1" (M.singleton "x" 1) (Add (Var "x") (Num 1)) 2
        ]

    testLet =
      testGroup
        "Let"
        [ testEvalNoVarSuccess "let x = 4 in x == 4" (Let "x" (Num 4) (Var "x")) 4,
          testEvalNoVarSuccess "let x = 4 in x + 3 == 7" (Let "x" (Num 4) (Add (Var "x") (Num 3))) 7,
          testEvalNoVarSuccess "let x = 4, let y = 2 in x * y == 8" (Let "x" (Num 4) (Let "y" (Num 2) (Mul (Var "x") (Var "y")))) 8,
          testEvalNoVarSuccess "let x = sqrt(16) in x == 4" (Let "x" (Sqrt (Num 16)) (Var "x")) 4,
          testFailure "let x = 4, let y = sqrt(-4) fails" M.empty (Let "x" (Num 4) (Let "y" (Sqrt (Num (-4))) (Add (Var "x") (Var "y")))),
          testEvalNoVarSuccess "let x = 2 in x ^ 2 == 4" (Let "x" (Num 2) (Pow (Var "x") (Num 2))) 4,
          testEvalNoVarSuccess "let x = 5 in let x = 6 in x == 6 (shadowing)" (Let "x" (Num 5) (Let "x" (Num 6) (Var "x"))) 6
        ]

    testNestedExpr =
      testGroup
        "Nested Expressions"
        [ testEvalNoVarSuccess "(2 + 3) * (5 - 2) == 15" (Mul (Add (Num 2) (Num 3)) (Sub (Num 5) (Num 2))) 15,
          testEvalNoVarSuccess "2 + (3 * (4 + 1)) == 17" (Add (Num 2) (Mul (Num 3) (Add (Num 4) (Num 1)))) 17
        ]

    testComplexExpr =
      testGroup
        "Complex Expressions"
        [ testEvalNoVarSuccess "let x = 3 in (x + 2) * (x - 1) == 10" 
            (Let "x" (Num 3) (Mul (Add (Var "x") (Num 2)) (Sub (Var "x") (Num 1)))) 10,
          
          testEvalNoVarSuccess "let x = 2 in let y = x + 3 in x * y == 10" 
            (Let "x" (Num 2) (Let "y" (Add (Var "x") (Num 3)) (Mul (Var "x") (Var "y")))) 10,
          
          testEvalNoVarSuccess "let x = 2 in let y = x * 2 in let z = y + 3 in z ^ x == 49" 
            (Let "x" (Num 2) 
              (Let "y" (Mul (Var "x") (Num 2)) 
                (Let "z" (Add (Var "y") (Num 3)) 
                  (Pow (Var "z") (Var "x"))))) 49
        ]

    testEvalNoVarSuccess msg expr res =
      testCase msg $ eval M.empty expr @?= Right res

    testSuccess msg state expr expected =
      testCase msg $
        case eval state expr of
          Right x -> assertBool "Evaluation result is wrong" (x == expected)
          Left _ -> assertFailure "Expression failed to evaluate"

    testFailure msg state expr =
      testCase msg $
        assertBool "Expr should fail to evaluate in the state" $
          isLeft $ eval state expr

main :: IO ()
main = defaultMain testEval
