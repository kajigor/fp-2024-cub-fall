module Main where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as Map

import Expr
import Error
import Interpreter

tests :: TestTree
tests = testGroup "Expression Evaluator Tests"
  [ testCase "Simple addition" $
      eval Map.empty (Add (Num 2) (Num 3)) @?= Right 5
  , testCase "Addition with negative numbers" $
      eval Map.empty (Add (Num (-5)) (Num 3)) @?= Right (-2)
  , testCase "Addition of zero" $
      eval Map.empty (Add (Num 0) (Num 5)) @?= Right 5

  , testCase "Subtraction" $
      eval Map.empty (Sub (Num 5) (Num 3)) @?= Right 2
  , testCase "Subtraction resulting in negative" $
      eval Map.empty (Sub (Num 3) (Num 5)) @?= Right (-2)
  , testCase "Subtraction with zero" $
      eval Map.empty (Sub (Num 5) (Num 0)) @?= Right 5

  , testCase "Multiplication" $
      eval Map.empty (Mul (Num 4) (Num 3)) @?= Right 12
  , testCase "Multiplication with zero" $
      eval Map.empty (Mul (Num 4) (Num 0)) @?= Right 0
  , testCase "Multiplication with negative number" $
      eval Map.empty (Mul (Num (-4)) (Num 3)) @?= Right (-12)

  , testCase "Division" $
      eval Map.empty (Div (Num 10) (Num 2)) @?= Right 5
  , testCase "Division resulting in float" $
    eval Map.empty (Div (Num 7) (Num 2)) @?= Right 3.5  
  , testCase "Division of zero" $
      eval Map.empty (Div (Num 0) (Num 5)) @?= Right 0

  , testCase "Power" $
      eval Map.empty (Pow (Num 2) (Num 3)) @?= Right 8
  , testCase "Power of zero exponent" $
      eval Map.empty (Pow (Num 2) (Num 0)) @?= Right 1
  , testCase "Power with base zero" $
      eval Map.empty (Pow (Num 0) (Num 3)) @?= Right 0

  , testCase "Square root" $
      eval Map.empty (Sqrt (Num 9)) @?= Right 3
  , testCase "Square root of zero" $
      eval Map.empty (Sqrt (Num 0)) @?= Right 0
  , testCase "Square root of fractional number" $
      eval Map.empty (Sqrt (Num 4)) @?= Right 2 

  , testCase "Complex expression" $
      eval Map.empty (Mul (Div (Num 4) (Num 2)) (Sqrt (Num 16))) @?= Right 8
  , testCase "More complex expression" $
      eval Map.empty (Add (Mul (Num 3) (Num 2)) (Sqrt (Num 25))) @?= Right 11
  , testCase "Complex with negative result" $
      eval Map.empty (Sub (Sqrt (Num 25)) (Mul (Num 2) (Num 3))) @?= Right (-1)

  , testCase "Division by zero" $
      eval Map.empty (Div (Num 1) (Num 0)) @?= Left (DivisionByZero (Num 0))
  , testCase "Division by zero in complex expression" $
      eval Map.empty (Add (Div (Num 1) (Num 0)) (Num 5)) @?= Left (DivisionByZero (Num 0))
  , testCase "Division by zero in nested expression" $
      eval Map.empty (Mul (Div (Num 4) (Num 0)) (Num 2)) @?= Left (DivisionByZero (Num 0))

  , testCase "Negative square root" $
      eval Map.empty (Sqrt (Num (-1))) @?= Left (NegativeSqrt (Num (-1)))
  , testCase "Negative square root inside expression" $
      eval Map.empty (Add (Num 2) (Sqrt (Num (-1)))) @?= Left (NegativeSqrt (Num (-1)))
  , testCase "Square root of negative let expression" $
      eval Map.empty (Let "x" (Num (-9)) (Sqrt (Var "x"))) @?= Left (NegativeSqrt (Num (-9)))

  , testCase "Variable substitution" $
      eval (Map.fromList [("x", 5)]) (Add (Var "x") (Num 3)) @?= Right 8
  , testCase "Variable substitution with zero" $
      eval (Map.fromList [("x", 0)]) (Add (Var "x") (Num 3)) @?= Right 3
  , testCase "Variable substitution with missing variable" $
      eval (Map.fromList [("y", 5)]) (Add (Var "x") (Num 3)) @?= Left (UnboundVariable "x")

  , testCase "Let expression" $
      eval Map.empty (Let "x" (Num 5) (Add (Var "x") (Num 3))) @?= Right 8
  , testCase "Let with nested expression" $
      eval Map.empty (Let "x" (Num 5) (Add (Var "x") (Mul (Var "x") (Num 2)))) @?= Right 15
  , testCase "Let overwriting variable" $
      eval Map.empty (Let "x" (Num 5) (Let "x" (Num 10) (Add (Var "x") (Num 3)))) @?= Right 13

  , testCase "Nested let expressions" $
      eval Map.empty (Let "x" (Num 5) (Let "y" (Add (Var "x") (Num 1)) (Mul (Var "x") (Var "y")))) @?= Right 30
  , testCase "Nested let with variable shadowing" $
      eval Map.empty (Let "x" (Num 5) (Let "x" (Num 2) (Mul (Var "x") (Num 3)))) @?= Right 6
  , testCase "Nested let with outer variable" $
      eval Map.empty (Let "x" (Num 5) (Let "y" (Num 3) (Add (Var "x") (Var "y")))) @?= Right 8

  , testCase "Unbound variable" $
      eval Map.empty (Var "z") @?= Left (UnboundVariable "z")
  , testCase "Unbound variable inside Let" $
      eval Map.empty (Let "x" (Num 5) (Add (Var "x") (Var "z"))) @?= Left (UnboundVariable "z")
  , testCase "Unbound variable in nested Let" $
      eval Map.empty (Let "x" (Num 5) (Let "y" (Var "z") (Add (Var "x") (Var "y")))) @?= Left (UnboundVariable "z")
  ]


main :: IO ()
main = defaultMain tests