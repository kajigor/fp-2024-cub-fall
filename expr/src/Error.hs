module Error (Error(..)) where

import Expr

data Error
  = DivisionByZero Expr
  | RootOfNegative Expr
  | ZeroToNegativePower Expr
  | UnknownVariable String 
  | VariableAlreadyDefined String
  deriving Eq

instance Show Error where
  show e = "ERROR! " ++ case e of
    DivisionByZero expr -> "Cannot divide by zero in \"" ++ show expr ++ "\""
    RootOfNegative expr -> "Cannot take a root of a negative number in \"" ++ show expr ++ "\""
    ZeroToNegativePower expr -> "Cannot raise 0 to a negative power in \"" ++ show expr ++ "\""
    UnknownVariable name -> "Unknown variable with the name " ++ "\"" ++ name ++ "\""
    VariableAlreadyDefined name -> "Variable \"" ++ name ++ "\" already defined"
    
    