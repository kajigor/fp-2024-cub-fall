module Error (Error(..)) where

import Expr (Expr)

data Error
  = DivByZero Expr Expr
  | NegativeSqrt Expr
  deriving (Eq, Show)