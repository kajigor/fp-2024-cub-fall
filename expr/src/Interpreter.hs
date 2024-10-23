module Interpreter (eval) where

import Expr 
import Error
import qualified Data.Map.Strict as Map

performOperation :: Map.Map String Double -> (Double -> Double -> Double) -> Expr -> Expr -> Either Error Double
performOperation state func a b = case (eval state a, eval state b) of
  (Left er, _) -> Left er 
  (_, Left er) -> Left er
  (Right x, Right y) -> Right (func x y)


eval :: Map.Map String Double -> Expr -> Either Error Double

eval _ (Num a) = Right a

eval state (Sqrt a) = case eval state a of
  Left x -> Left x
  Right x -> if x < 0 then Left (NegRoot (Sqrt a)) else Right (sqrt x) 

eval state (Add a b) = performOperation state (+) a b

eval state (Sub a b) = performOperation state (-) a b

eval state (Mul a b) = performOperation state (*) a b

eval state (Div a b) = case eval state b of
  Right 0 -> Left (DivZero (Div a b))
  _ -> performOperation state (/) a b

eval state (Pow a b) = performOperation state (**) a b

eval state (Var name) = case Map.lookup name state of
  Nothing -> Left (EmptyVariable name)
  Just value -> Right value

eval state (Let name body expression) = case eval state body of
  Left e -> Left e
  Right value -> case eval (Map.insert name value state) expression of
    Left er -> Left er
    Right valueLet -> Right valueLet
