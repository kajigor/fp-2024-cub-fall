import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as Map
import qualified Error
import qualified Expr
import qualified Interpreter

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ basicTests
  , variableTests
  , letTests
  , errorTests
  ]

basicTests :: TestTree
basicTests = testGroup "Basic Operations"
  [ testCase "Addition" $
      Interpreter.eval Map.empty (Expr.Add (Expr.Num 2) (Expr.Num 3)) @?= Right 5
  , testCase "Multiplication" $
      Interpreter.eval Map.empty (Expr.Mul (Expr.Num 4) (Expr.Num 3)) @?= Right 12
  , testCase "Power" $
      Interpreter.eval Map.empty (Expr.Pow (Expr.Num 2) (Expr.Num 3)) @?= Right 8
  , testCase "Sqrt" $
      Interpreter.eval Map.empty (Expr.Sqrt (Expr.Num 9)) @?= Right 3
  , testCase "Complex" $
      Interpreter.eval Map.empty (Expr.Mul (Expr.Div (Expr.Num 4) (Expr.Num 2)) (Expr.Sqrt (Expr.Num 16))) @?= Right 8
  ]

variableTests :: TestTree
variableTests = testGroup "Variable Operations"
  [ testCase "Loookup" $
      Interpreter.eval (Map.fromList [("x", 5)]) (Expr.Var "x") @?= Right 5
  , testCase "Variable" $
      Interpreter.eval (Map.fromList [("x", 5)]) (Expr.Add (Expr.Var "x") (Expr.Num 3)) @?= Right 8
  , testCase "Unbound variable" $
      Interpreter.eval Map.empty (Expr.Var "x") @?= Left (Error.UnboundVariable "x")
  ]

letTests :: TestTree
letTests = testGroup "Let Tests"
  [ testCase "Simple let" $
      Interpreter.eval Map.empty (Expr.Let "x" (Expr.Num 5) (Expr.Var "x")) @?= Right 5
  , testCase "Nested let" $
      Interpreter.eval Map.empty 
        (Expr.Let "x" (Expr.Num 13)
          (Expr.Let "y" (Expr.Add (Expr.Var "x") (Expr.Num 1))
            (Expr.Pow (Expr.Var "y") (Expr.Num 2)))) @?= Right 196
  , testCase "Shadowing let" $
      Interpreter.eval Map.empty
        (Expr.Let "x" (Expr.Num 5)
          (Expr.Let "x" (Expr.Add (Expr.Var "x") (Expr.Num 1))
            (Expr.Var "x"))) @?= Right 6
  ]

errorTests :: TestTree
errorTests = testGroup "Error Tests"
  [ testCase "Negative square root" $
      Interpreter.eval Map.empty (Expr.Sqrt (Expr.Num (-1))) @?= Left (Error.NegativeSqrt (Expr.Sqrt (Expr.Num (-1))))
  , testCase "Division by zero" $
      Interpreter.eval Map.empty (Expr.Div (Expr.Num 1) (Expr.Num 0)) @?= Left (Error.DivisionByZero (Expr.Div (Expr.Num 1) (Expr.Num 0)))
  , testCase "Unbound variable" $
      Interpreter.eval Map.empty (Expr.Add (Expr.Var "x") (Expr.Num 1)) @?= Left (Error.UnboundVariable "x")
  ]