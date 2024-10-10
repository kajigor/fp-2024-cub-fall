module Error ( Error(..)) where
import Expr (Expr)

data Error = 
    NegativeSqrt Expr
  | DivisionByZero Expr
  | UnboundVariable String
  deriving (Eq, Show)

instance Show Error where
  show (NegativeSqrt e) = "Invalid: Negative square root in expression: " ++ show e
  show (DivisionByZero e) = "Invalid: Division by zero in expression: " ++ show e
  show (UnboundVariable e) = "Invalid: Unbound variable in expression: " ++ show e