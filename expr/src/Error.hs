module Error (Error(..)) where

import Expr (Expr)

data Error
  = DivByZero Expr Expr
  | NegativeSqrt Expr
  | UndefinedVar String
  deriving (Eq)

instance Show Error where
  show (DivByZero expr1 expr2) = 
    "Error: Division by zero encountered when evaluating " ++ show expr1 ++ " / " ++ show expr2
  show (NegativeSqrt expr) = 
    "Error: Attempted to compute the square root of a negative value: " ++ show expr
  show (UndefinedVar var) = 
    "Error: Undefined variable: " ++ var