module Expr where

data Expr = Num Double
            | Var String
            | Plus Expr Expr
            | Minus Expr Expr
            | Mult Expr Expr
            | Div Expr Expr
            | Pow Expr Expr
            | Abs Expr
            | UnaryMinus Expr

instance Show Expr where
    show (Num x) = show x
    show (Var s) = s
    show (Plus e1 e2) = "(" ++ show e1 ++ ") + (" ++ show e2 ++ ")"
    show (Minus e1 e2) = "(" ++ show e1 ++ ") - (" ++ show e2 ++ ")"
    show (Mult e1 e2) = "(" ++ show e1 ++ ") * (" ++ show e2 ++ ")"
    show (Div e1 e2) = "(" ++ show e1 ++ ") / (" ++ show e2 ++ ")"
    show (Pow e1 e2) = "(" ++ show e1 ++ ") ** (" ++ show e2 ++ ")"
    show (Abs e) = "abs(" ++ show e ++ ")"
    show (UnaryMinus e) = "-(" ++ show e ++ ")"

data SimpleEvalError = DivByZero
                 | ZeroNonPositivePow Double
                 | NegNonNaturalPow Double Double
                 | UnknownVariable String
    deriving Eq

data EvalError = EvalError SimpleEvalError Expr

instance Show EvalError where
    show (EvalError simpleError expr) = errorMsg simpleError ++
                 " (while evaluating the expression " ++ show expr ++ ")" where
        errorMsg DivByZero = "error: division by zero"
        errorMsg (ZeroNonPositivePow x) = "error: raising 0 to non-positive power " ++ show x
        errorMsg (NegNonNaturalPow x y) = "error: raising negative (" ++ show x ++ ") to non-natural power " ++ show y
        errorMsg (UnknownVariable name) = "error: unknown variable: " ++ name
