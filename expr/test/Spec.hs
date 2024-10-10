
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as M
import Expr
import Interpreter
import Error

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" 
  [ testCase "Basic addition" $
      eval M.empty (Add (Num 1) (Num 2)) @?= Right 3

  , testCase "Let expression" $
      eval M.empty (Let "x" (Num 3) (Add (Var "x") (Num 4))) @?= Right 7

  , testCase "Nested let expression" $
      eval M.empty (Let "x" (Num 2) (Let "y" (Add (Var "x") (Num 3)) (Pow (Var "y") (Num 2)))) @?= Right 25

  , testCase "Undefined variable" $
      eval M.empty (Var "y") @?= Left (UndefinedVariable "y")

  , testCase "Division by zero" $
      eval M.empty (Div (Num 4) (Num 0)) @?= Left (DivByZero (Div (Num 4) (Num 0)))

  , testCase "Division by zero in nested expression" $
      eval M.empty (Add (Div (Num 4) (Num 0)) (Num 5)) @?= Left (DivByZero (Div (Num 4) (Num 0)))

  , testCase "Square root of negative number" $
      eval M.empty (Sqrt (Num (-4))) @?= Left (NegativeSqrt (Num (-4)))

  , testCase "Square root of zero" $
      eval M.empty (Sqrt (Num 0)) @?= Right 0

  , testCase "Square root of positive number" $
      eval M.empty (Sqrt (Num 16)) @?= Right 4

  , testCase "Power of a number" $
      eval M.empty (Pow (Num 2) (Num 3)) @?= Right 8

  , testCase "Power of negative base" $
      eval M.empty (Pow (Num (-2)) (Num 3)) @?= Right (-8)

  , testCase "Complex expression with let, power and sqrt" $
      eval M.empty (Let "x" (Num 4) (Mul (Sqrt (Var "x")) (Pow (Num 2) (Num 3)))) @?= Right 16

  , testCase "Let-expression with undefined variable" $
      eval M.empty (Let "x" (Num 4) (Add (Var "y") (Num 2))) @?= Left (UndefinedVariable "y")

  , testCase "Add and subtract complex expression" $
      eval M.empty (Sub (Add (Num 3) (Num 7)) (Mul (Num 2) (Num 3))) @?= Right 4
  ]
