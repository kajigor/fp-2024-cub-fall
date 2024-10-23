module Error where

import Expr ( Expr (..))

data Error
  = DivisionByZero Expr
  | NegativeSqrt Expr
  | Unknownvariable String
  deriving Eq

instance Show Error where
    show (DivisionByZero expr) = "Cannot divide by zero in \"" ++ show expr ++ "\""
    show (NegativeSqrt expr) = "Cannot take a root of a negative number in \"" ++ show expr ++ "\""
    show (Unknownvariable var) = "Unknown variable with the name " ++ "\"" ++ var ++ "\""