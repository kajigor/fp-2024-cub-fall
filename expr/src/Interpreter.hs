module Interpreter (eval) where

import qualified Data.Map as Map
import qualified Error as E
import qualified Expr as Ex

eval :: Map.Map String Double -> Ex.Expr -> Either E.Error Double
eval env expr = case expr of
  Ex.Num x -> Right x
  Ex.Var v -> case Map.lookup v env of
    Just val -> Right val
    Nothing -> Left (E.UnboundVariable v)
  Ex.Let var e1 e2 -> do
    val1 <- eval env e1
    eval (Map.insert var val1 env) e2
  Ex.Sqrt e -> do
    x <- eval env e
    if x < 0
      then Left (E.NegativeSqrt expr)
      else Right (sqrt x)
  Ex.Div e1 e2 -> do
    x <- eval env e1
    y <- eval env e2
    if y == 0
      then Left (E.DivisionByZero expr)
      else Right (x / y)
  Ex.Add e1 e2 -> do
    x <- eval env e1
    y <- eval env e2
    Right (x + y)
  Ex.Sub e1 e2 -> do
    x <- eval env e1
    y <- eval env e2
    Right (x - y)
  Ex.Mul e1 e2 -> do
    x <- eval env e1
    y <- eval env e2
    Right (x * y)
  Ex.Pow e1 e2 -> do
    x <- eval env e1
    y <- eval env e2
    Right (x ** y)