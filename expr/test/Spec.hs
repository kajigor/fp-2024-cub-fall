module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Expr
import Error
import Interpreter

import qualified Data.Map as Map

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Evaluator Tests"
    [ testCase "Addition" $
        eval Map.empty (Binary OpAdd (Lit 2) (Lit 3)) @?= Right 5.0

    , testCase "Division by zero" $
        eval Map.empty (Binary OpDiv (Lit 1) (Lit 0)) @?= Left (ErrDivByZero (Binary OpDiv (Lit 1) (Lit 0)))

    , testCase "Square root of negative number" $
        eval Map.empty (Unary "sqrt" (Lit (-4))) @?= Left (ErrNegativeSqrt (Unary "sqrt" (Lit (-4))))

    , testCase "Undefined variable" $
        eval Map.empty (Var "x") @?= Left (ErrUndefinedVar "x")

    , testCase "Let binding and usage" $
        let expr = Let "x" (Lit 5) (Binary OpMul (Var "x") (Lit 2))
        in eval Map.empty expr @?= Right 10.0

    , testCase "Variable redefinition error" $
        let env = Map.fromList [("x", 1)]
            expr = Let "x" (Lit 2) (Var "x")
        in eval env expr @?= Left (ErrVarRedefinition "x")

    , testCase "Invalid power operation (zero to negative exponent)" $
        eval Map.empty (Binary OpPow (Lit 0) (Lit (-1))) @?= Left (ErrInvalidOperation "0 raised to negative exponent" (Binary OpPow (Lit 0) (Lit (-1))))

    , testCase "Invalid power operation (negative base with non-integer exponent)" $
        eval Map.empty (Binary OpPow (Lit (-2)) (Lit 0.5)) @?= Left (ErrInvalidOperation "Negative base with non-integer exponent" (Binary OpPow (Lit (-2)) (Lit 0.5)))

    , testCase "Nested let expressions" $
        let expr = Let "x" (Lit 2) (Let "y" (Binary OpAdd (Var "x") (Lit 3)) (Binary OpMul (Var "x") (Var "y")))
        in eval Map.empty expr @?= Right 10.0
    ]
