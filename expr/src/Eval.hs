module Eval (eval) where

import qualified Data.Map.Strict as M
import Expr
import Error


eval :: M.Map String Double -> Expr -> Either Error Double
eval _ (Lit x) = Right x
eval state (Sqrt sub) = case eval state sub of
  Right x
    | x < 0 -> Left (NegativeRoot (Sqrt sub))
    | otherwise -> Right (sqrt x)
  Left e -> Left e

eval state (Let k v x) = case eval state v of
  Right v -> eval (M.insert k v state) x
  Left e -> Left e

eval state (Var v) = do
  case M.lookup v state of
    Just v -> Right v
    Nothing -> Left (UndefinedVariable v)

eval state expr =
  let 
    evalBinOp op x y = 
      case (eval state x, eval state y) of
        (Right x, Right y) -> Right (op x y)
        (Left e, _) -> Left e
        (_, Left e) -> Left e
  in case expr of
    Add x y -> evalBinOp (+) x y
    Sub x y -> evalBinOp (-) x y
    Mul x y -> evalBinOp (*) x y
    Div x y -> case eval state y of
      Right y | y == 0 -> Left (DivisionByZero expr)
      _ -> evalBinOp (/) x y
    Pow x y -> case (eval state x, eval state y) of 
      (Right x, Right y) | x < 0 && 0 < abs y && abs y < 1 -> Left (NegativeRoot expr)
      _ -> evalBinOp (**) x y
