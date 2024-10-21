module Interpreter
  ( eval
  ) where

import qualified Data.Map as Map
import Expr (Expr(..))
import Error (Error(..))

eval :: Map.Map String Double -> Expr -> Either Error Double
eval _ (Num x) = Right x
eval env (Var s) = case Map.lookup s env of
  Just val -> Right val
  Nothing -> Left $ UnboundVariable s
eval env (Let var expr body) = do
  value <- eval env expr
  eval (Map.insert var value env) body
eval env (Sqrt expr) = do
  x <- eval env expr
  if x < 0 
    then Left $ NegativeSqrt (Num x)
    else Right (sqrt x)
eval env (Add e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  Right (x + y)
eval env (Sub e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  Right (x - y)
eval env (Mul e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  Right (x * y)
eval env (Div e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  if y == 0 
    then Left $ DivisionByZero e2
    else Right (x / y)
eval env (Pow e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  Right (x ** y)