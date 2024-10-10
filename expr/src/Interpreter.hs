module Interpreter (eval) where

import qualified Data.Map.Strict as M

import Error (Error(..))
import Expr (Expr(..))


evalBinary :: (Double -> Double -> Double) -> M.Map String Double -> Expr -> Expr -> Either Error Double
evalBinary op env x y = do
  valX <- eval env x
  valY <- eval env y
  return (valX `op` valY)

eval :: M.Map String Double -> Expr -> Either Error Double
eval _ (Number x) = Right x

eval env (Var x) =
  case M.lookup x env of
    Nothing -> Left (UndefinedVariable x)
    Just val -> Right val

eval env (Sqrt x) = do
  val <- eval env x
  if val < 0
    then Left (NegativeSqrt x)
    else Right (sqrt val)

eval env (Add x y)      = evalBinary (+)  env x y
eval env (Subtract x y) = evalBinary (-)  env x y
eval env (Multiply x y) = evalBinary (*)  env x y
eval env (Power x y)    = evalBinary (**) env x y

eval env (Divide x y) = do
  valY <- eval env y
  if valY == 0
    then Left (DivisionByZero (Divide x y))
    else evalBinary (/) env x y

eval env (Let var expr body) = do
  val <- eval env expr
  eval (M.insert var val env) body
