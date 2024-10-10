module Error where

import Expr

data Error
  = DivByZero Expr
  | NegativeSqrt Expr
  | UndefinedVariable String
  | InvalidPow Expr
  | OtherError String
  deriving (Eq)

instance Show Error where
  show (DivByZero expr) = "Division by zero in: " ++ show expr
  show (NegativeSqrt expr) = "Square root of a negative number in: " ++ show expr
  show (InvalidPow expr) = "Invalid power operation in: " ++ show expr
  show (UndefinedVariable msg) = "Undefined variable found: " ++ msg
  show (OtherError msg) = "Error: " ++ msg
