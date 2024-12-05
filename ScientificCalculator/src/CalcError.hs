module CalcError where

import Expr

data CalcErrorType
  = DivisionByZero
  | NegativeSqrt
  | LogNonPositive
  | InvalidLogBase
  | DomainError
  | InvalidFactorial
  deriving (Show, Eq)

data CalcError = CalcError
  { errorType :: CalcErrorType
  , expression :: Expr
  } deriving (Show, Eq)