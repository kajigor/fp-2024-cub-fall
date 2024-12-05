module Evaluator (
    evaluate
) where

import Parser
import Memory
import Control.Monad.State
import qualified Data.Map as Map

-- Evaluate an expression
evaluate :: Expr -> CalcState -> (Double, CalcState)
evaluate expr calcState = runState (evalExpr expr) calcState

evalExpr :: Expr -> State CalcState Double
evalExpr (Num x) = return x
evalExpr (Var name) = do
    st <- get
    case Map.lookup name (memory st) of
        Just val -> return val
        Nothing -> error $ "Variable '" ++ name ++ "' not found in memory."
evalExpr (Add x y) = (+) <$> evalExpr x <*> evalExpr y
evalExpr (Sub x y) = (-) <$> evalExpr x <*> evalExpr y
evalExpr (Mul x y) = (*) <$> evalExpr x <*> evalExpr y
evalExpr (Div x y) = do
    divisor <- evalExpr y
    if divisor == 0
        then return (1 / 0) -- Return Infinity or some other default value
        else (/) <$> evalExpr x <*> pure divisor
evalExpr (Pow x y) = (**) <$> evalExpr x <*> evalExpr y
evalExpr (Sqrt x) = do
    value <- evalExpr x
    if value < 0
        then return (0/0)
        else return (sqrt value)
evalExpr (Neg x) = negate <$> evalExpr x
evalExpr (Sin x) = sin <$> evalExpr x
evalExpr (Cos x) = cos <$> evalExpr x
evalExpr (Tan x) = tan <$> evalExpr x
evalExpr (Log x) = logBase 10 <$> evalExpr x
evalExpr (Ln x) = log <$> evalExpr x
evalExpr Pi = return pi
evalExpr E = return (exp 1)
