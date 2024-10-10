module Error(Error(..)) where

import Text.Printf (printf)

import Expr
    
data Error = NegativeSqrt Expr
     | ZeroDiv Expr
     | Unbounded String
     deriving(Eq)

instance Show Error where
  show (NegativeSqrt expr) = printf "Error: Square root of a negative number in expression: %s" (show expr)
  show (ZeroDiv expr) = printf "Error: Division by 0 is not possible in expression: %s" (show expr)
  show (Unbounded var) = printf "Unbouded variable: %s" (show var)
