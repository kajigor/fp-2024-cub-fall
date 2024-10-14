import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Expr
import Error
import Interpreter
import qualified Data.Map.Strict as M

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Expression Tests"
  [ testNumbers
  , testVariables
  , testLetExpressions
  , testOperators
  , testErrors
  , testComplexExpressions
  ]

testNumbers :: TestTree
testNumbers = testGroup "Number Evaluation"
  [ testCase "Evaluate Number 3.0" $
      eval M.empty (Number 3.0) @?= Right 3.0
  ]

testVariables :: TestTree
testVariables = testGroup "Variable Evaluation"
  [ testCase "Evaluate Var x when x = 5.0" $
      let env = M.fromList [("x", 5.0)]
      in eval env (Var "x") @?= Right 5.0
  , testCase "Evaluate Var y when y is undefined" $
      let env = M.fromList [("x", 5.0)]
      in eval env (Var "y") @?= Left (UndefinedVariable "y")
  ]

testLetExpressions :: TestTree
testLetExpressions = testGroup "Let Expression Evaluation"
  [ testCase "Evaluate let x = 5.0 in x + 2.0" $
      eval M.empty (Let "x" (Number 5.0) (Plus (Var "x") (Number 2.0))) @?= Right 7.0
  , testCase "Evaluate nested let-expressions" $
      eval M.empty (Let "x" (Number 13.0)
        (Let "y" (Plus (Var "x") (Number 1.0))
          (Exp (Var "y") (Number 2.0)))) @?= Right 196.0
  ]

testOperators :: TestTree
testOperators = testGroup "Operator Evaluation"
  [ testCase "Addition" $
      eval M.empty (Plus (Number 1.0) (Number 10.0)) @?= Right 11.0
  , testCase "Subtraction" $
      eval M.empty (Minus (Number 2.3) (Number 4.8)) @?= Right (-2.5)
  , testCase "Multiplication" $
      eval M.empty (Prod (Number 4.0) (Number 2.5)) @?= Right 10.0
  , testCase "Division" $
      eval M.empty (Divide (Number 1.0) (Number 4.0)) @?= Right 0.25
  , testCase "Exponentiation" $
      eval M.empty (Exp (Number 2.0) (Number 10.0)) @?= Right 1024.0
  ]

testErrors :: TestTree
testErrors = testGroup "Error Handling"
  [ testCase "Division by zero" $
      let expr = Divide (Number 5.0) (Minus (Number 4.0) (Number 4.0))
      in eval M.empty expr @?= Left (DivisionByZero expr)
  , testCase "Root of negative number" $
      let expr = Sqrt (Number (-9.0))
      in eval M.empty expr @?= Left (RootOfNegative expr)
  , testCase "Zero to negative power" $
      let expr = Exp (Number 0.0) (Number (-1.0))
      in eval M.empty expr @?= Left (ZeroToNegativePower expr)
  , testCase "Undefined variable in let body" $
      let expr = Let "x" (Number 5.0) (Plus (Var "x") (Var "y"))
      in eval M.empty expr @?= Left (UndefinedVariable "y")
  ]

testComplexExpressions :: TestTree
testComplexExpressions = testGroup "Complex Expressions"
  [ testCase "Evaluate complex expression with variables and let" $
      let expr = Let "x" (Number 3.4)
                    (Divide (Plus (Var "x") (Number 6.6)) (Sqrt (Number 25.0)))
      in eval M.empty expr @?= Right 2.0
  , testCase "Evaluate complex expression with variables and let" $
      let expr = Let "n" (Number 9.0)
                    (Let "m" (Number 5.0)
                      (Plus (Prod (Number 2.0) (Sqrt (Var "n"))) (Minus (Number 10.0) (Var "m"))))
      in eval M.empty expr @?= Right 11.0
  ]