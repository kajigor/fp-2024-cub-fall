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

instance Eq Expr where
  (==) = exprEquals

exprEquals :: Expr -> Expr -> Bool
exprEquals (Num a) (Num b) = a == b
exprEquals (Var a) (Var b) = a == b
exprEquals (Plus a1 b1) (Plus a2 b2) = exprEquals a1 a2 && exprEquals b1 b2 || exprEquals a1 b2 && exprEquals b1 a2
exprEquals (Minus a1 b1) (Minus a2 b2) = exprEquals a1 a2 && exprEquals b1 b2
exprEquals (Mult a1 b1) (Mult a2 b2) = exprEquals a1 a2 && exprEquals b1 b2 || exprEquals a1 b2 && exprEquals b1 a2
exprEquals (Div a1 b1) (Div a2 b2) = exprEquals a1 a2 && exprEquals b1 b2
exprEquals (Pow a1 b1) (Pow a2 b2) = exprEquals a1 a2 && exprEquals b1 b2
exprEquals (Abs a1) (Abs a2) = exprEquals a1 a2
exprEquals (UnaryMinus (Num a)) (Num b) = a == -b
exprEquals (Num a) (UnaryMinus (Num b)) = a == -b
exprEquals (UnaryMinus a1) (UnaryMinus a2) = exprEquals a1 a2
exprEquals _ _ = False

instance Show Expr where
    show (Num x) = show x
    show (Var s) = "\"" ++ s ++ "\""
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
    deriving Eq

instance Show EvalError where
    show (EvalError simpleError expr) = errorMsg simpleError ++
                 " (while evaluating the expression " ++ show expr ++ ")" where
        errorMsg DivByZero = "error: division by zero"
        errorMsg (ZeroNonPositivePow x) = "error: raising 0 to non-positive power " ++ show x
        errorMsg (NegNonNaturalPow x y) = "error: raising negative (" ++ show x ++ ") to non-natural power " ++ show y
        errorMsg (UnknownVariable name) = "error: unknown variable: " ++ name
