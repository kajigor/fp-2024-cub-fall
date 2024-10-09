module Error(Error(..)) where

import qualified Expr as e
    
data Error = NegativeSqrt e.Expr
     | ZeroDiv e.Expr

instance Show Error where
  show (NegativeSqrt expr) = printf "Error: Square root of a negative number in expression: %s" (show expr)
  show (ZeroDiv expr) = printf "Error: Division by 0 is not possible in expression: %s" (show expr)

instance Eq Error where
  NegativeSqrt expr1 == NegativeSqrt expr2 = expr1 == expr2
  ZeroDiv expr1 == ZeroDiv expr2 = expr1 == expr2
  _ == _ = False
