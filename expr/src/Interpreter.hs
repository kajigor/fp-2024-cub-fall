module Interpreter(eval) where

import Control.Monad (unless)
import Text.Printf (printf)
import qualified Data.Map as Map
import qualified Expr
import qualified Error

evalBinary :: (Double -> Double -> Double) -> Map.Map String Double -> Expr.Expr -> Expr.Expr -> Either Error.Error Double
evalBinary op cx x y = do
    ex <- eval cx x
    ey <- eval cx y
    return (ex `op` ey)

eval :: Map.Map String Double -> Expr.Expr -> Either Error.Error Double
eval _ (Expr.Num x)     = Right x
eval cx (Expr.Add x y)  = evalBinary (+) cx x y
eval cx (Expr.Sub x y)  = evalBinary (-) cx x y
eval cx (Expr.Mul x y)  = evalBinary (*) cx x y
eval cx (Expr.Div x y)  = do
    ex <- eval cx x
    ey <- eval cx y
    if ey == 0
        then Left (Error.DivByZero x y)
        else return (ex / ey)
eval cx (Expr.Pow x y)  = evalBinary (**) cx x y
eval cx (Expr.Sqrt x)   = do
    ex <- eval cx x
    if ex < 0
        then Left (Error.NegativeSqrt x)
        else return (sqrt ex)
eval cx (Expr.Variable x) = 
    case Map.lookup x cx of
        Just value -> Right value
        Nothing    -> Left (Error.UnassignedVariable x)
eval cx (Expr.Let a b c) = do
    aVal <- eval cx b
    eval (Map.insert a aVal cx) c
    