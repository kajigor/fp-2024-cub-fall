module Error (Error(..)) where
import Expr (Expr)


data Error
  = NegativeRoot Expr
  | DivisionByZero Expr
  | UndefinedVariable String
  deriving (Eq)

instance Show Error where
  show (NegativeRoot expr) = "Root of negative number: " ++ show expr
  show (DivisionByZero expr) = "Division by zero: " ++ show expr
  show (UndefinedVariable v) = "Undefined variable: " ++ v
