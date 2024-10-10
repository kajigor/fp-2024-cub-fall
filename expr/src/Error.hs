module Error (Error(..)) where

import Expr (Expr)


data Error
  = NegativeSqrt Expr
  | DivisionByZero Expr
  | UndefinedVariable String
  deriving (Eq)

instance Show Error where
  show (NegativeSqrt expr)     = "Error: Square root of a negative number: \"" ++ show expr ++ "\""
  show (DivisionByZero expr)   = "Error: Division by zero: \"" ++ show expr ++ "\""
  show (UndefinedVariable var) = "Error: Undefined variable: \"" ++ var ++ "\""
