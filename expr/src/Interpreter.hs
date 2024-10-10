module Interpreter (eval) where

import Expr
import Error
import qualified Data.Map as Map
import Data.Map (Map)

eval :: Map String Double -> Expr -> Either EvalError Double
eval _ (Lit n) = Right n

eval env (Var x) =
    case Map.lookup x env of
        Just val -> Right val
        Nothing  -> Left (ErrUndefinedVar x)

eval env (Unary op expr) = do
    val <- eval env expr
    case op of
        "sqrt" -> if val >= 0
                    then Right (sqrt val)
                    else Left (ErrNegativeSqrt (Unary op expr))
        _ -> Left (ErrInvalidOperation ("Unknown unary operator: " ++ op) (Unary op expr))

eval env (Binary op left right) = do
    lVal <- eval env left
    rVal <- eval env right
    case op of
        OpAdd  -> Right (lVal + rVal)
        OpSub  -> Right (lVal - rVal)
        OpMul  -> Right (lVal * rVal)
        OpDiv  -> if rVal == 0
                    then Left (ErrDivByZero (Binary op left right))
                    else Right (lVal / rVal)
        OpPow  -> handlePower lVal rVal (Binary op left right)
  where
    handlePower base exponent expr
        | base == 0 && exponent < 0 = Left (ErrInvalidOperation "0 raised to negative exponent" expr)
        | base < 0 && not (isInteger exponent) = Left (ErrInvalidOperation "Negative base with non-integer exponent" expr)
        | otherwise = Right (base ** exponent)
    isInteger x = x == fromInteger (round x)

eval env (Let var expr body) =
    if Map.member var env
        then Left (ErrVarRedefinition var)
        else do
            val <- eval env expr
            let env' = Map.insert var val env
            eval env' body
