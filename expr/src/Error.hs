module Error (Error (..)) where
import Expr

data Error = 
  DivZero Expr 
  | NegRoot Expr
  | EmptyVariable String 
  deriving Eq 

instance Show Error where
  show (DivZero expr) = "division by zero is forbidden " ++ show expr
  show (NegRoot expr) = "sqrt of a negative number is forbidden " ++ show expr
  show (EmptyVariable name) = "empty variable " ++ name
