import qualified Data.Map.Strict as M
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import Interpreter (eval)
import Error (Error(..))
import Expr (Expr (..))


-- Auxiliary functions:

testEvalNoVarSuccess :: String -> Expr -> Double -> TestTree
testEvalNoVarSuccess msg expr res =
  testCase msg $ eval M.empty expr @?= Right res

testSuccess :: String -> M.Map String Double -> Expr -> Double -> TestTree
testSuccess msg state expr expected =
  testCase msg $
    case eval state expr of
      Right x -> assertBool "Evaluation result is wrong" (x == expected)
      Left _ -> assertFailure "Expression failed to evaluate"

testError :: String -> Error -> M.Map String Double -> Expr -> TestTree
testError msg expectedError state expr =
  testCase msg $
    case eval state expr of
      Left err | err == expectedError -> return ()
      Left err -> assertFailure $ "Expected " ++ show expectedError ++ ", got: " ++ show err
      Right _ -> assertFailure "Expected an error but got a result"

-- Tests:

testAdd :: TestTree
testAdd =
  testGroup
    "Add"
    [ testEvalNoVarSuccess "1 + 2 == 3" (Add (Number 1) (Number 2)) 3,
      testEvalNoVarSuccess "5 + (-3) == 2" (Add (Number 5) (Number (-3))) 2,
      testEvalNoVarSuccess "Nested: (1 + 2) + (3 + 4) == 10"
        (Add (Add (Number 1) (Number 2)) (Add (Number 3) (Number 4))) 10
    ]

testSubtract :: TestTree
testSubtract =
  testGroup
    "Subtract"
    [ testEvalNoVarSuccess "5 - 3 == 2" (Subtract (Number 5) (Number 3)) 2,
      testEvalNoVarSuccess "5 - (-3) == 8" (Subtract (Number 5) (Number (-3))) 8,
      testEvalNoVarSuccess "Nested: (10 - 3) - (5 - 1) == 3"
        (Subtract (Subtract (Number 10) (Number 3)) (Subtract (Number 5) (Number 1))) 3
    ]

testMultiply :: TestTree
testMultiply =
  testGroup
    "Multiply"
    [ testEvalNoVarSuccess "3 * 4 == 12" (Multiply (Number 3) (Number 4)) 12,
      testEvalNoVarSuccess "2 * (-3) == -6" (Multiply (Number 2) (Number (-3))) (-6),
      testEvalNoVarSuccess "Nested: (2 * 3) * (4 * 5) == 120"
        (Multiply (Multiply (Number 2) (Number 3)) (Multiply (Number 4) (Number 5))) 120
    ]

testDivide :: TestTree
testDivide =
  testGroup
    "Divide"
    [ testEvalNoVarSuccess "6 / 2 == 3" (Divide (Number 6) (Number 2)) 3,
      testEvalNoVarSuccess "5 / 2 == 2.5" (Divide (Number 5) (Number 2)) 2.5,
      testError "Division by zero" (DivisionByZero (Divide (Number 5) (Number 0))) M.empty
        (Divide (Number 5) (Number 0))
    ]

testPower :: TestTree
testPower =
  testGroup
    "Power"
    [ testEvalNoVarSuccess "2 ^ 3 == 8" (Power (Number 2) (Number 3)) 8,
      testEvalNoVarSuccess "3 ^ (-2) == 0.11(1)" (Power (Number 3) (Number (-2))) (1 / 9),
      testEvalNoVarSuccess "Nested: (2 ^ 3) ^ 2 == 64" (Power (Power (Number 2) (Number 3)) (Number 2)) 64
    ]

testSqrt :: TestTree
testSqrt =
  testGroup
    "Sqrt"
    [ testEvalNoVarSuccess "sqrt(9) == 3" (Sqrt (Number 9)) 3,
      testError "Square root of negative number" (NegativeSqrt (Number (-16))) M.empty (Sqrt (Number (-16))),
      testEvalNoVarSuccess "Nested: sqrt(sqrt(16)) == 2" (Sqrt (Sqrt (Number 16))) 2
    ]

testVar :: TestTree
testVar =
  testGroup
    "Var"
    [ testSuccess "x + 1 == 3, x == 2" (M.singleton "x" 2) (Add (Var "x") (Number 1)) 3,
      testError "x + 1 fails when x isn't assigned" (UndefinedVariable "x") M.empty (Add (Var "x") (Number 1)),
      testSuccess "Nested variables: x + y == 5, x == 2, y == 3" (M.fromList [("x", 2), ("y", 3)])
        (Add (Var "x") (Var "y")) 5
    ]

testLet :: TestTree
testLet =
  testGroup
    "Let"
    [ testEvalNoVarSuccess "let x = 2 in x + 3 == 5" (Let "x" (Number 2) (Add (Var "x") (Number 3))) 5,
      testEvalNoVarSuccess "let x = 1, y = x + 1 in y * 2 == 4"
        (Let "x" (Number 1) (Let "y" (Add (Var "x") (Number 1)) (Multiply (Var "y") (Number 2)))) 4,
      testEvalNoVarSuccess "let x = 2 in (let y = x + 1 in y + x) == 5"
        (Let "x" (Number 2) (Let "y" (Add (Var "x") (Number 1)) (Add (Var "y") (Var "x")))) 5
    ]

testComplexExpr :: TestTree
testComplexExpr =
  testGroup
    "Complex Expressions"
    [ testEvalNoVarSuccess "Complex expr: ((2 + 3) * (4 - 1)) / 5 == 3"
        (Divide (Multiply (Add (Number 2) (Number 3)) (Subtract (Number 4) (Number 1))) (Number 5)) 3,

      testEvalNoVarSuccess "Complex nested expr: (2 * (3 + (4 / 2))) - (5 ^ 2) == -15"
        (Subtract
          (Multiply (Number 2) (Add (Number 3) (Divide (Number 4) (Number 2))))
          (Power (Number 5) (Number 2))) (-15),

      testEvalNoVarSuccess "Complex let expr: let x = 2, y = x + 1 in (y * (x + 3)) - (x ^ 2) == 11"
        (Let "x" (Number 2)
          (Let "y" (Add (Var "x") (Number 1))
            (Subtract (Multiply (Var "y") (Add (Var "x") (Number 3))) (Power (Var "x") (Number 2))))) 11,

      testEvalNoVarSuccess "Complex let with nested: let x = 3 in let y = (x + 1) * 2 in ((y - x) / 2) ^ 2 == 6.25"
        (Let "x" (Number 3)
          (Let "y" (Multiply (Add (Var "x") (Number 1)) (Number 2))
            (Power (Divide (Subtract (Var "y") (Var "x")) (Number 2)) (Number 2)))) 6.25,

      testEvalNoVarSuccess "Super complex expr: ((2 ^ 3) + (sqrt(16) * 5)) / (let x = 3 in x + 4) == 4"
        (Divide
          (Add (Power (Number 2) (Number 3)) (Multiply (Sqrt (Number 16)) (Number 5)))
          (Let "x" (Number 3) (Add (Var "x") (Number 4)))) 4
    ]

testErrors :: TestTree
testErrors =
  testGroup
    "Errors"
    [ testError "Division by zero"
        (DivisionByZero (Divide (Number 4) (Number 0))) M.empty (Divide (Number 4) (Number 0)),
      testError "Undefined variable: y"
        (UndefinedVariable "y") M.empty (Var "y"),
      testError "Square root of negative number"
        (NegativeSqrt (Number (-16))) M.empty (Sqrt (Number (-16))),
      testError "Let with undefined variable: let x = y + 1 in x + 2 fails"
        (UndefinedVariable "y") M.empty (Let "x" (Add (Var "y") (Number 1)) (Add (Var "x") (Number 2)))
    ]

-- Running:

testEval :: TestTree
testEval =
  testGroup "Eval"
    [ testAdd
    , testSubtract
    , testMultiply
    , testDivide
    , testPower
    , testSqrt
    , testVar
    , testLet
    , testComplexExpr
    , testErrors
    ]

main :: IO ()
main =
  defaultMain $ testGroup "Expressions" [testEval]
