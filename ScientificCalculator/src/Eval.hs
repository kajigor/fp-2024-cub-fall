module Eval where

import Expr
import CalcError

evalInfix :: Expr -> Expr -> (Double -> Double -> Double) -> Either CalcError Double
evalInfix x y f = f <$> eval x <*> eval y

eval :: Expr -> Either CalcError Double
eval (Num x) = return x


eval (Add x y) = evalInfix x y (+)
eval (Mul x y) = evalInfix x y (*)
eval (Diff x y) = evalInfix x y (-)
eval (Pow x y) = do 
    result <- evalInfix x y (**)
    if isNaN result then Left $ CalcError DomainError (Pow x y) else Right result
    

eval (Div x y) = do
    denom <- eval y
    if denom == 0
        then Left $ CalcError DivisionByZero (Div x y)
        else flip (/) denom <$> eval x


eval (Sqrt x) = do
    expr <- eval x
    if expr < 0
        then Left $ CalcError NegativeSqrt (Sqrt x)
        else return (sqrt expr)


eval (Cbrt x) = (** (1 / 3)) <$> eval x


eval (Reciprocal x) = do
    denom <- eval x
    if denom == 0
        then Left $ CalcError DivisionByZero (Reciprocal x)
        else return (1 / denom)


eval (Square x) = (** 2) <$> eval x
eval (Cube x) = (** 3) <$> eval x


eval (Ln x) = do
    expr <- eval x
    if expr <= 0
        then Left $ CalcError LogNonPositive (Ln x)
        else return (log expr)

eval (Log10 x) = do
    expr <- eval x
    if expr <= 0
        then Left $ CalcError LogNonPositive (Log10 x)
        else return (logBase 10 expr)


eval (LogBase base x) = do
    baseVal <- eval base
    expr <- eval x
    if baseVal <= 0 || baseVal == 1
        then Left $ CalcError InvalidLogBase (LogBase base x)
        else if expr <= 0
            then Left $ CalcError LogNonPositive (LogBase base x)
            else return (logBase baseVal expr)


eval (Exp x) = exp <$> eval x


eval (Sin x) = sin  <$> eval x
eval (Cos x) = cos  <$> eval x
eval (Tan x) = tan  <$> eval x


eval (ASin x) = do
    expr <- eval x
    if expr < -1 || expr > 1
        then Left $ CalcError DomainError (ASin x)
        else return (asin expr)

eval (ACos x) = do
    expr <- eval x
    if expr < -1 || expr > 1
        then Left $ CalcError DomainError (ACos x)
        else return (acos expr)

eval (ATan x) = atan <$> eval x


eval (Sinh x) = sinh <$> eval x
eval (Cosh x) = cosh <$> eval x
eval (Tanh x) = tanh <$> eval x


eval (ASinh x) = asinh <$> eval x

eval (ACosh x) = do
    expr <- eval x
    if expr < 1
        then Left $ CalcError DomainError (ACosh x)
        else return (acosh expr)

eval (ATanh x) = do
    expr <- eval x
    if expr <= -1 || expr >= 1
        then Left $ CalcError DomainError (ATanh x)
        else return (atanh expr)


eval (Factorial x) = do
    expr <- eval x
    if expr < 0 || fromIntegral (truncate expr) /= expr
        then Left $ CalcError InvalidFactorial (Factorial x)
        else return (factorial 1 (fromIntegral (truncate expr)))
  where
    factorial acc 0 = acc
    factorial acc 1 = acc
    factorial acc n = factorial (acc * n) (n - 1)


eval Pi = return pi
eval E = return (exp 1)


eval (Exp10 base exponent) = do
    baseVal <- eval base
    expVal <- eval exponent
    return (baseVal * (10 ** expVal))


eval (Rad x) = toRadians <$> eval x
eval (Deg x) = toDegrees <$> eval x


toRadians :: Double -> Double
toRadians deg = deg * (pi / 180)


toDegrees :: Double -> Double
toDegrees rad = (rad * 180) / pi
