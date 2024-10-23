module Interpreter where

import Expr ( Expr (..))
import Error ( Error(..) )
import qualified Data.Map as Map

type Env = Map.Map String Double

eval :: Env -> Expr -> Either Error Double
eval _ (Number n) = Right n

eval env (Var x) =
  case Map.lookup x env of
    Just val -> Right val
    Nothing -> Left (Unknownvariable x)

eval env (SquareRoot e) =
  case eval env e of
    Right x | x < 0 -> Left (NegativeSqrt e)
            | otherwise -> Right (sqrt x)
    Left err -> Left err

eval env (Add e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  return (x + y)

eval env (Subtract e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  return (x - y)

eval env (Multiply e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  return (x * y)

eval env (Divide e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  if y == 0
    then Left (DivisionByZero e2)
    else return (x / y)

eval env (Power e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  return (x ** y)