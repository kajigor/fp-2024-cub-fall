module Eval where

import Expr
import CalcError

evalInfix :: Expr -> Expr -> (Double -> Double -> Double) -> Either CalcError Double
evalInfix x y f = f <$> eval x <*> eval y

eval :: Expr -> Either CalcError Double
eval (Num x) = return x

-- Basic arithmetic operations
eval (Add x y) = evalInfix x y (+)
eval (Mul x y) = evalInfix x y (*)
eval (Diff x y) = evalInfix x y (-)
eval (Pow x y) = do 
    result <- evalInfix x y (**)
    if isNaN result then Left $ CalcError DomainError (Pow x y) else Right result
    
-- Division with division by zero handling
eval (Div x y) = do
    denom <- eval y
    if denom == 0
        then Left $ CalcError DivisionByZero (Div x y)
        else flip (/) denom <$> eval x

-- Square root with negative value handling
eval (Sqrt x) = do
    expr <- eval x
    if expr < 0
        then Left $ CalcError NegativeSqrt (Sqrt x)
        else return (sqrt expr)

-- Cube root
eval (Cbrt x) = (** (1 / 3)) <$> eval x

-- Reciprocal with division by zero handling
eval (Reciprocal x) = do
    denom <- eval x
    if denom == 0
        then Left $ CalcError DivisionByZero (Reciprocal x)
        else return (1 / denom)

-- Square and cube
eval (Square x) = (** 2) <$> eval x
eval (Cube x) = (** 3) <$> eval x

-- Logarithms
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

-- Logarithm with base y
eval (LogBase base x) = do
    baseVal <- eval base
    expr <- eval x
    if baseVal <= 0 || baseVal == 1
        then Left $ CalcError InvalidLogBase (LogBase base x)
        else if expr <= 0
            then Left $ CalcError LogNonPositive (LogBase base x)
            else return (logBase baseVal expr)

-- Exponential
eval (Exp x) = exp <$> eval x

-- Trigonometric functions (interpreted in degrees by default, converted to radians)
eval (Sin x) = sin  <$> eval x
eval (Cos x) = cos  <$> eval x
eval (Tan x) = tan  <$> eval x

-- Inverse trigonometric functions
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

-- Hyperbolic trigonometric functions
eval (Sinh x) = sinh <$> eval x
eval (Cosh x) = cosh <$> eval x
eval (Tanh x) = tanh <$> eval x

-- Inverse hyperbolic trigonometric functions
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

-- Factorial (only valid for non-negative integers)
eval (Factorial x) = do
    expr <- eval x
    if expr < 0 || fromIntegral (truncate expr) /= expr
        then Left $ CalcError InvalidFactorial (Factorial x)
        else return (factorial 1 (fromIntegral (truncate expr)))
  where
    factorial acc 0 = acc
    factorial acc 1 = acc
    factorial acc n = factorial (acc * n) (n - 1)

-- Constants
eval Pi = return pi
eval E = return (exp 1)

-- Scientific notation (EE)
eval (Exp10 base exponent) = do
    baseVal <- eval base
    expVal <- eval exponent
    return (baseVal * (10 ** expVal))

-- Angle mode (defaults to degrees, converted to radians)
eval (Rad x) = toRadians <$> eval x
eval (Deg x) = toDegrees <$> eval x

-- Convert degrees to radians
toRadians :: Double -> Double
toRadians deg = deg * (pi / 180)

-- converts radians to degrees
toDegrees :: Double -> Double
toDegrees rad = (rad * 180) / pi
