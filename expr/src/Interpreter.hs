module Interpreter where

import qualified Data.Map.Strict as M
import Err
import Expr

evalBinOp :: M.Map String Double -> (Double -> Double -> Double) -> Expr -> Expr -> Either Error Double
evalBinOp state op a b = case (eval state a, eval state b) of
  (Left err, _) -> Left err
  (_, Left err) -> Left err
  (Right aVal, Right bVal) -> Right (aVal `op` bVal)

eval :: M.Map String Double -> Expr -> Either Error Double
eval _ (Numb a) = Right a
eval state (Var a) = case M.lookup a state of
  Just val -> Right val
  Nothing -> Left (VarNotFound (Var a))
eval state (SqrtExpr a) = case eval state a of
  Left err -> Left err
  Right aVal -> if aVal < 0 then Left (SqrtNeg (SqrtExpr a)) else Right (sqrt aVal)
eval state (BinOpAdd a b) = evalBinOp state (+) a b
eval state (BinOpSub a b) = evalBinOp state (-) a b
eval state (BinOpMul a b) = evalBinOp state (*) a b
eval state (BinOpDiv a b) = case eval state b of
  (Left x) -> Left x
  (Right x) -> if x == 0 then Left (DivByZero (BinOpDiv a b)) else evalBinOp state (/) a b
eval state (BinOpPow a b) = evalBinOp state (**) a b
eval state (LetExpr name val stmt) = case eval state val of
  Left err -> Left err
  Right valDouble -> case eval state' stmt of
    Left err -> Left err
    Right a -> Right a
    where
      state' = M.insert name valDouble state