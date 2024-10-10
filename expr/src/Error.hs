module Error (EvalError(..)) where

import Expr

data EvalError
    = ErrDivByZero Expr
    | ErrNegativeSqrt Expr
    | ErrUndefinedVar String
    | ErrVarRedefinition String
    | ErrInvalidOperation String Expr
    deriving (Eq)

instance Show EvalError where
    show (ErrDivByZero expr) = "Division by zero in expression: " ++ show expr
    show (ErrNegativeSqrt expr) = "Square root of negative number in expression: " ++ show expr
    show (ErrUndefinedVar var) = "Undefined variable: " ++ var
    show (ErrVarRedefinition var) = "Variable '" ++ var ++ "' is already defined"
    show (ErrInvalidOperation msg expr) = "Invalid operation (" ++ msg ++ ") in expression: " ++ show expr
