module Error(Error(..)) where

import qualified Expr

data Error
  = DivByZero Expr.Expr Expr.Expr
  | NegativeSqrtNum Expr.Expr
  | UnassignedVar String
  deriving (Eq)

instance Show Error where
  show (DivByZero a b) = "Division by zero error: " ++ show a ++ " / " ++ show b
  show (NegativeSqrtNum s) = "Cannot take square root of negative number: " ++ show s
  show (UnassignedVar s) = "Unassigned variables: " ++ s