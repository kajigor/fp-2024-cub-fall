module Err where

import Expr
import Text.Printf (printf)

data Error = DivByZero Expr | SqrtNeg Expr | VarNotFound Expr
  deriving (Eq)

instance Show Error where
  show (DivByZero s) = printf "Expression evaluation failed in %s. Second arg for div cannot be zero.\n" (show s)
  show (SqrtNeg s) = printf "Expression evaluation failed in %s. Arg for sqrt cannot be negative.\n" (show s)
  show (VarNotFound s) = printf "Expression evaluation failed in %s. Error 404. Undefined variable.\n" (show s)
