module Expr (
  Expr,
  eval,
  run,
) where

import Lib
import qualified Data.Map.Strict as M
import BaseExpr

instance Num Expr where
  (+) = Plus
  (*) = Mult
  abs = Abs
  signum = Sgn
  negate = UnaryMinus

addContext :: Expr -> (Double -> Double -> Either SimpleError Double) -> Double -> Double -> Either Error Double
addContext expr f x y = either (\res -> Left $ Error res expr) Right (f x y)

evalBinary :: (Double -> Double -> Either Error Double) -> (Either Error Double) -> (Either Error Double) -> (Either Error Double)
evalBinary f _ (Left e) = Left e
evalBinary f (Left e) _ = Left e
evalBinary f (Right x) (Right y) = f x y

wrap :: (Double -> Double -> Double) -> Double -> Double -> Either Error Double
wrap f x y = Right $ f x y

safeDiv :: Double -> Double -> Either SimpleError Double
safeDiv x y | y == 0 = Left DivByZero
            | otherwise = Right $ x / y

safePow :: Double -> Double -> Either SimpleError Double
safePow x y | (x == 0 && y <= 0) = Left $ ZeroNonPositivePow y
            | (x < 0 && (y < 0 || y /= fromIntegral (floor y))) = Left $ NegNonNaturalPow x y
            | otherwise = Right $ x ** y

eval :: M.Map String Expr -> Expr -> Either Error Double
eval _ (Number n) = Right n
eval state expr@(Var name) = do
  case M.lookup name state of
    Just name -> eval state name
    Nothing -> Left $ Error (UnknownVariable name) expr
eval state (Let name val expr) = eval (M.insert name val state) expr
eval state expr@(Sqrt x) = evalBinary (addContext expr safePow) (eval state x) (Right 0.5)
eval state (Plus x y) = evalBinary (wrap (+)) (eval state x) (eval state y)
eval state (Minus x y) = evalBinary (wrap (-)) (eval state x) (eval state y)
eval state (Mult x y) = evalBinary (wrap (*)) (eval state x) (eval state y)
eval state expr@(Div x y) = evalBinary (addContext expr safeDiv) (eval state x) (eval state y)
eval state expr@(Pow x y) = evalBinary (addContext expr safePow) (eval state x) (eval state y)
eval state (Abs x) = evalBinary (wrap (\x y -> abs x)) (eval state x) (Right 0)
eval state (Sgn x) = evalBinary (wrap (\x y -> signum x)) (eval state x) (Right 0)
eval state (UnaryMinus x) = evalBinary (wrap (\x y -> (-x))) (eval state x) (Right 0)

run :: Expr -> M.Map String Expr -> IO ()
run expr state = do
  print expr
  print state
  print (eval state expr)
  putStrLn ""

main = do
  let expr1 = Var "x"
  let expr2 = Plus (Number 2) (Number 2)
  let expr3 = Plus (Var "x") (Number 1)
  let state1 = M.fromList [("x", Number 42), ("y", Number 13)]
  let state2 = M.empty
  run expr1 state1
  run expr2 state1
  run expr3 state1
  run expr1 state2
  run expr2 state2
  run expr3 state2
