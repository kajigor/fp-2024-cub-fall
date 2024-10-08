module Error(Error(..)) where

import Text.Printf (printf)
import qualified Expr

data Error = DivByZero Expr.Expr Expr.Expr
            | NegativeSqrt Expr.Expr
            | UnassignedVar String

instance Show Error where
  show (DivByZero a b ) = printf "Cannot divide %s by %s" (show a) (show b)
  show (NegativeSqrt a ) = printf "Cannot take the square root of a negative number: %s" (show a)
  show (UnassignedVar s) = printf "Unassigned variables: %s" (show s)

instance Eq Error where
  DivByZero _ _ == DivByZero _ _ = True
  NegativeSqrt _ == NegativeSqrt _ = True
  UnassignedVar _ == UnassignedVar _ = True
  _ == _ = False
