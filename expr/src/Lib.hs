module Lib where

import BaseExpr (Expr)

data SimpleError = DivByZero
                 | ZeroNonPositivePow Double
                 | NegNonNaturalPow Double Double
                 | UnknownVariable String
  deriving Eq

data Error = Error SimpleError Expr
  deriving Eq

instance Show Error where
  show (Error simpleError expr) = (errorMsg simpleError) ++
                 " (while evaluating the expression " ++ (show expr) ++ ")"
    where errorMsg DivByZero = "error: division by zero"
          errorMsg (ZeroNonPositivePow x) = "error: raising 0 to non-positive power " ++ show x
          errorMsg (NegNonNaturalPow x y) = "error: raising negative (" ++ show x ++ ") to non-natural power " ++ show y
          errorMsg (UnknownVariable name) = "error: unknown variable: " ++ name
