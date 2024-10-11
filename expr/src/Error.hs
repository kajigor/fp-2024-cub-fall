module Error(Error(..)) where
import Expr
import Text.Printf (printf)

data Error = 
  DividedByZero Expr
  | SqrtOfNegative Expr
  | NegativeExponent Expr
  | NotVariable String
  | MultipleDeclaration String
  deriving (Eq)

instance Show Error where
  show (DividedByZero expr) = printf "Any expression divided by 0 is undefined. In the expression: %s" (show expr)
  show (SqrtOfNegative expr) = printf "Taking a square root of a negative number is undefined. In the expression: %s" (show expr)
  show (NegativeExponent expr) = printf "Number raised to the power of a negative number is undefined. In the expression: %s" (show expr)
  show (NotVariable var) = printf "Not a proper variable. In the string: %s" var
  show (MultipleDeclaration var) = printf "Variable %s is already defined. Multiple declarations are not allowed." var