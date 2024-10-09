module Error (Error (..)) where

import qualified Expr
import Text.Printf (printf)

data Error
  = DivisionByZero Expr.Expr Expr.Expr
  | SqrtOfNegNumber Expr.Expr
  | UnassignedVar String

instance Show Error where
  show (DivisionByZero a b) = printf "Error: tried to do division by zero: %s / %s" (show a) (show b)
  show (SqrtOfNegNumber a) = printf "Error: tried to find the square root of a negative number: sqrt %s" (show a)
  show (UnassignedVar a) = printf "Error: unassigned variable %s" (show a)

instance Eq Error where
  DivisionByZero a c == DivisionByZero b d = a == b && c == d
  SqrtOfNegNumber a == SqrtOfNegNumber b = a == b
  UnassignedVar a == UnassignedVar b = a == b
  _ == _ = False