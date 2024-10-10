module Interpreter where

import qualified Data.Map as M
import Expr
import Error

eval :: M.Map String Double -> Expr -> Either Error Double
eval _ (Num x) = Right x
eval env (Var x) = case M.lookup x env of
  Just v  -> Right v
  Nothing -> Left (UndefinedVariable x)
eval env (Sqrt e) = do
  x <- eval env e
  if x < 0 then Left (NegativeSqrt e) else Right (sqrt x)
eval env (Add e1 e2) = (+) <$> eval env e1 <*> eval env e2
eval env (Sub e1 e2) = (-) <$> eval env e1 <*> eval env e2
eval env (Mul e1 e2) = (*) <$> eval env e1 <*> eval env e2
eval env (Div e1 e2) = do
  x <- eval env e2
  if x == 0 then Left (DivByZero (Div e1 e2)) else (/) <$> eval env e1 <*> Right x
eval env (Pow e1 e2) = (**) <$> eval env e1 <*> eval env e2
eval env (Let var val body) = do
  value <- eval env val
  eval (M.insert var value env) body
