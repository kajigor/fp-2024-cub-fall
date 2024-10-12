module Interpreter ( eval )where

import Expr
import Error
import qualified Data.Map as M

evalBinaryOperands :: M.Map String Double -> Expr -> Expr -> (Double -> Double -> Double)-> Either Error Double
evalBinaryOperands mp a b op =
  case eval mp a of
    Left err -> Left err
    Right eval_a ->
      case eval mp b of
        Left err -> Left err
        Right eval_b -> Right (op eval_a eval_b)

eval :: M.Map String Double -> Expr -> Either Error Double
eval _ (Number x)= Right x
eval mp expr@(Sqrt x) =
  case eval mp x of
  Left err -> Left err
  Right eval_x
      | eval_x >= 0 -> Right (sqrt eval_x)
      | otherwise -> Left (RootOfNegative expr)
eval mp (Plus a b) = evalBinaryOperands mp a b (+)
eval mp (Minus a b) = evalBinaryOperands mp a b (-)
eval mp (Prod a b) = evalBinaryOperands mp a b (*)
eval mp expr@(Divide a b) =
  case evalBinaryOperands mp a b (/) of 
    Left err -> Left err
    Right res
      | isInfinite res -> Left (DivisionByZero expr)
      | otherwise -> Right res
eval mp expr@(Exp a b) =
  case evalBinaryOperands mp a b (**) of 
    Left err -> Left err
    Right res
      | isNaN res -> Left (RootOfNegative expr)
      | isInfinite res -> Left (ZeroToNegativePower expr)
      | otherwise -> Right res
eval mp (Variable name) = 
  case M.lookup name mp of
    Just value -> Right value
    Nothing -> Left (UnknownVariable name)
eval mp (Let name eq_value in_expr) 
  | M.member name mp = Left (VariableAlreadyDefined name)
  | otherwise = 
    case eval mp eq_value of 
    Left err -> Left err
    Right value -> 
      case eval (M.insert name value mp) in_expr of
        Left err -> Left err
        Right val -> Right val
