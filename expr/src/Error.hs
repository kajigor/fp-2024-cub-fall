module Error where

import Expr

data Error
  = DivisionByZero Expr
  | RootOfNegative Expr
  | ZeroToNegativePower Expr
  | UndefinedVariable String
  deriving (Eq)

instance Show Error where
  show (DivisionByZero expr) = "ERROR: Cannot divide by zero in \"" ++ show expr ++ "\""
  show (RootOfNegative expr) = "ERROR: Cannot take the square root of a negative number in \"" ++ show expr ++ "\""
  show (ZeroToNegativePower expr) = "ERROR: Cannot raise 0 to a negative power in \"" ++ show expr ++ "\""
  show (UndefinedVariable var) = "ERROR: Undefined variable \"" ++ var ++ "\""
