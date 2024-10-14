module Interpreter where

import Expr
import Error
import qualified Data.Map.Strict as M

type Env = M.Map String Double

eval :: Env -> Expr -> Either Error Double
eval _ (Number x) = Right x

eval env (Var x) =
  case M.lookup x env of
    Just val -> Right val
    Nothing -> Left (UndefinedVariable x)

eval env (Let x expr body) = do
  val <- eval env expr
  let newEnv = M.insert x val env
  eval newEnv body

eval env expr@(Sqrt x) = do
  val <- eval env x
  if val >= 0
    then Right (sqrt val)
    else Left (RootOfNegative expr)

eval env (Plus a b) = do
  valA <- eval env a
  valB <- eval env b
  Right (valA + valB)

eval env (Minus a b) = do
  valA <- eval env a
  valB <- eval env b
  Right (valA - valB)

eval env (Prod a b) = do
  valA <- eval env a
  valB <- eval env b
  Right (valA * valB)

eval env expr@(Divide a b) = do
  valA <- eval env a
  valB <- eval env b
  if valB == 0
    then Left (DivisionByZero expr)
    else Right (valA / valB)

eval env expr@(Exp a b) = do
  valA <- eval env a
  valB <- eval env b
  if valA == 0 && valB < 0
    then Left (ZeroToNegativePower expr)
    else if valA < 0 && not (isInteger valB)
      then Left (RootOfNegative expr)
      else Right (valA ** valB)
  where
    isInteger x = x == fromInteger (round x)
