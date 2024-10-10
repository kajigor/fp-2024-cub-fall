module Error where

import Control.Monad (unless)
import qualified Data.Map.Strict as M
import Text.Printf (printf)

import Expr
    
data Error = NegativeSqrt Expr
     | ZeroDiv Expr
     | Unbounded String

instance Show Error where
  show (NegativeSqrt expr) = printf "Error: Square root of a negative number in expression: %s" (show expr)
  show (ZeroDiv expr) = printf "Error: Division by 0 is not possible in expression: %s" (show expr)
  show (Unbounded var) = printf "Unbouded variable: %s" (show var)

instance Eq Error where
  NegativeSqrt expr1 == NegativeSqrt expr2 = expr1 == expr2
  ZeroDiv expr1 == ZeroDiv expr2 = expr1 == expr2s
  Unbounded var1 = Unbounded var2 = var1 == var2
  _ == _ = False
