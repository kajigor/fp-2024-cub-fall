module Error(Error(..)) where

import Text.Printf (printf)
import qualified Expr

data Error = DivByZero Expr.Expr Expr.Expr
            | NegativeSqrt Expr.Expr
            | UnassignedVariable String
            deriving(Eq)

instance Show Error where
  show (DivByZero a b ) = printf "Cannot divide %s by %s" (show a) (show b)
  show (NegativeSqrt a ) = printf "Cannot take the square root of a negative number: %s" (show a)
  show (UnassignedVariable s) = printf "Unassigned variable: %s" (show s)
