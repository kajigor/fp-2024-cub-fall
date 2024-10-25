import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as Map

import Expr (Expr(..))
import Error (Error(..))
import Interpreter (eval)

testCases :: TestTree
testCases = testGroup "Expression Evaluator Tests"
  [ testCase "Num 8" $
      eval Map.empty (Num 8) @?= Right 8

  , testCase "Add 4 + 5" $
      eval Map.empty (Add (Num 4) (Num 5)) @?= Right 9

  , testCase "Sub 20 - 6" $
      eval Map.empty (Sub (Num 20) (Num 6)) @?= Right 14

  , testCase "Mul 3 * 4" $
      eval Map.empty (Mul (Num 3) (Num 4)) @?= Right 12

  , testCase "Div 20 / 4" $
      eval Map.empty (Div (Num 20) (Num 4)) @?= Right 5

  , testCase "Div by zero" $
      eval Map.empty (Div (Num 1) (Num 0)) @?= Left (DivByZero (Num 1) (Num 0))

  , testCase "Sqrt of 36" $
      eval Map.empty (Sqrt (Num 36)) @?= Right 6

  , testCase "Sqrt of negative number" $
      eval Map.empty (Sqrt (Num (-9))) @?= Left (NegativeSqrt (Num (-9)))

  , testCase "Pow 4^2" $
      eval Map.empty (Pow (Num 4) (Num 2)) @?= Right 16

  , testCase "Undefined variable" $
      eval Map.empty (Var "y") @?= Left (UndefinedVar "y")

  , testCase "Let x = 15 in x" $
      eval (Map.fromList [("x", 15)]) (Let "x" (Num 15) (Var "x")) @?= Right 15

  , testCase "Let x = 15 in let y = x + 3 in y ^ 2" $
      eval (Map.fromList [("x", 15)]) (Let "x" (Num 15) (Let "y" (Add (Var "x") (Num 3)) (Pow (Var "y") (Num 2)))) @?= Right 324

  , testCase "Shadowed variable" $
      eval (Map.fromList [("x", 3)]) (Let "x" (Num 3) (Let "x" (Num 4) (Add (Var "x") (Num 5)))) @?= Right 9
  ]

main :: IO ()
main = defaultMain testCases