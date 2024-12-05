module Eval (evalExpr) where

import Expr
import qualified Data.Map.Strict as M

type CalcState = M.Map String Double

addContext :: Expr -> (Double -> Double -> Either SimpleEvalError Double) -> Double -> Double -> Either EvalError Double
addContext expr f x y = either (\res -> Left $ EvalError res expr) Right (f x y)

evalBinary :: (Double -> Double -> Either EvalError Double) -> Either EvalError Double -> Either EvalError Double -> Either EvalError Double
evalBinary _ _ (Left e) = Left e
evalBinary _ (Left e) _ = Left e
evalBinary f (Right x) (Right y) = f x y

wrap :: (Double -> Double -> Double) -> Double -> Double -> Either EvalError Double
wrap f x y = Right $ f x y

safeDiv :: Double -> Double -> Either SimpleEvalError Double
safeDiv x y | y == 0 = Left DivByZero
            | otherwise = Right $ x / y

safePow :: Double -> Double -> Either SimpleEvalError Double
safePow x y | x == 0 && y <= 0 = Left $ ZeroNonPositivePow y
            | x < 0 && (y < 0 || y /= fromIntegral (floor y :: Integer)) = Left $ NegNonNaturalPow x y
            | otherwise = Right $ x ** y

evalExpr :: CalcState -> Expr -> Either EvalError Double
evalExpr _ (Num x) = Right x
evalExpr state expr@(Var var) = case M.lookup var state of
    Nothing -> Left $ EvalError (UnknownVariable var) expr
    Just value -> Right value
evalExpr state (Plus x y) = evalBinary (wrap (+)) (evalExpr state x) (evalExpr state y)
evalExpr state (Minus x y) = evalBinary (wrap (-)) (evalExpr state x) (evalExpr state y)
evalExpr state (Mult x y) = evalBinary (wrap (*)) (evalExpr state x) (evalExpr state y)
evalExpr state expr@(Div x y) = evalBinary (addContext expr safeDiv) (evalExpr state x) (evalExpr state y)
evalExpr state expr@(Pow x y) = evalBinary (addContext expr safePow) (evalExpr state x) (evalExpr state y)
evalExpr state (Abs x) = evalBinary (wrap (flip $ const abs)) (evalExpr state x) (Right 0)
evalExpr state (UnaryMinus x) = evalBinary (wrap (flip $ const negate)) (evalExpr state x) (Right 0)
