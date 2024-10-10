module Spec where

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Expr  
import Error
import Interpreter

assigned :: Map String Double
assigned = Map.fromList [("x", 10.0), ("y", 8.0), ("z", -1.0)]

tests :: TestTree
tests = testGroup "Coverage Tests"
  [ testCase "Number literal" $
      eval Map.empty (Num 3) @?= Right 3.0

  , testCase "Square root of positive number" $
      eval Map.empty (Sqrt (Num 9)) @?= Right 3.0

  , testCase "Square root of negative number" $
      eval Map.empty (Sqrt (Num (-9))) @?= Left (NegativeSqrt (Num (-9)))

  , testCase "Addition of two numbers" $
      eval Map.empty (CompExpr Add (Num 3) (Num 7)) @?= Right 10.0

  , testCase "Subtraction of two numbers" $
      eval Map.empty (CompExpr Sub (Num 7) (Num 3)) @?= Right 4.0
  
  , testCase "Subtraction of two numbers negative" $
      eval Map.empty (CompExpr Sub (Num 3) (Num 7)) @?= Right (-4.0)

  , testCase "Multiplication of two numbers" $
      eval Map.empty (CompExpr Mult (Num 2) (Num 10)) @?= Right 20.0

  , testCase "Division of two numbers" $
      eval Map.empty (CompExpr Div (Num 9) (Num 3)) @?= Right 3.0

  , testCase "Division by 0" $
      eval Map.empty (CompExpr Div (Num 3) (Num 0)) @?= Left (ZeroDiv (CompExpr Div (Num 3) (Num 0)))

  , testCase "Power" $
      eval Map.empty (CompExpr Pow (Num 3) (Num 3)) @?= Right 27.0

  , testCase "Composite sqrt" $
      eval Map.empty (Sqrt (CompExpr Sub (Num 7) (Num 3))) @?= Right 2.0

  , testCase "Composite sqrt error" $
      eval Map.empty (Sqrt (CompExpr Sub (Num 3) (Num 7))) @?= Left (NegativeSqrt (CompExpr Sub (Num 3) (Num 7)))

  , testCase "Composite addition" $
      eval Map.empty (CompExpr Add (CompExpr Mult (Num 2) (Num 10)) (CompExpr Sub (Num 10) (Num 2))) @?= Right 28.0

  , testCase "Composite division error" $
      eval Map.empty (CompExpr Div (CompExpr Mult (Num 2) (Num 10)) (CompExpr Sub (Num 10) (Num 10))) @?= Left (ZeroDiv (CompExpr Div (CompExpr Mult (Num 2) (Num 10)) (CompExpr Sub (Num 10) (Num 10))))

  , testCase "Pow division by 0" $
      eval Map.empty (CompExpr Pow (Num 0) (Num (-4))) @?= Left (ZeroDiv (CompExpr Pow (Num 0) (Num (-4))))

  , testCase "Pow negative sqrt" $
      eval Map.empty (CompExpr Pow (Num (-10)) (Num 0.5)) @?= Left (NegativeSqrt (CompExpr Pow (Num (-10)) (Num 0.5)))

  , testCase "Variable evaluation (x)" $
      eval assigned (Var "x") @?= Right 10.0

  , testCase "Expression with variables (x + y)" $
      eval assigned (CompExpr Add (Var "x") (Var "y")) @?= Right 18.0

  , testCase "Let expression evaluation" $
      eval Map.empty (Let "x" (Num 7) (CompExpr Add (Var "x") (Num 5))) @?= Right 12.0

  , testCase "Complex expression with variables and operations" $
      eval assigned (CompExpr Add (CompExpr Mult (Var "x") (Num 2)) (CompExpr Sub (Var "y") (Num 1))) @?= Right 27.0

  , testCase "Let expression introduces a new variable" $
      eval Map.empty (Let "d" (Num 1) (CompExpr Add (Var "d") (Num 5))) @?= Right 6.0  

  , testCase "Let x = 13 in Let y = x + 1 in y ** 2" $
    eval Map.empty (Let "x" (Num 13) 
                   (Let "y" (CompExpr Add (Var "x") (Num 1)) 
                   (CompExpr Pow (Var "y") (Num 2)))) @?= Right 196.0
  
  , testCase "Unbound variable" $
      eval assigned (Var "a") @?= Left (Unbound "a") 

  , testCase "Variable division by 0" $
      eval assigned (Let "p" (Num 0) (CompExpr Div (Var "x") (Var "p"))) @?= Left (ZeroDiv (CompExpr Div (Var "x") (Var "p")))

  , testCase "Variable negative sqrt" $
      eval assigned (Sqrt (Var "z")) @?= Left (NegativeSqrt (Var "z"))
  ]

main :: IO ()
main =
  defaultMain $ testGroup "Expressions" [tests]
