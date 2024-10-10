import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as Map

import Expr (Expr(..))
import Error (Error(..))
import Interpreter (eval)

testCases :: TestTree
testCases = testGroup "Expression Evaluator Tests"
  [ testCase "Num 5" $
      eval (Num 5) @?= Right 5

  , testCase "Add 2 + 3" $
      eval (Add (Num 2) (Num 3)) @?= Right 5

  , testCase "Sub 10 - 4" $
      eval (Sub (Num 10) (Num 4)) @?= Right 6

  , testCase "Mul 3 * 3" $
      eval (Mul (Num 3) (Num 3)) @?= Right 9

  , testCase "Div 10 / 2" $
      eval (Div (Num 10) (Num 2)) @?= Right 5

  , testCase "Div by zero" $
      eval (Div (Num 1) (Num 0)) @?= Left (DivByZero (Num 1) (Num 0))

  , testCase "Sqrt of 16" $
      eval (Sqrt (Num 16)) @?= Right 4

  , testCase "Sqrt of negative number" $
      eval (Sqrt (Num (-4))) @?= Left (NegativeSqrt (Num (-4)))

  , testCase "Pow 2^3" $
      eval (Pow (Num 2) (Num 3)) @?= Right 8

  , testCase "Undefined variable" $
      eval (Var "x") @?= Left (UndefinedVariable "x")

  , testCase "Let x = 13 in x" $
      eval (Let "x" (Num 13) (Var "x")) @?= Right 13

  , testCase "Let x = 13 in let y = x + 1 in y ^ 2" $
      eval (Let "x" (Num 13) (Let "y" (Add (Var "x") (Num 1)) (Pow (Var "y") (Num 2)))) @?= Right 196

  , testCase "Shadowed variable" $
      eval (Let "x" (Num 3) (Let "x" (Num 4) (Add (Var "x") (Num 2)))) @?= Right 6
  ]

main :: IO ()
main = defaultMain testCases