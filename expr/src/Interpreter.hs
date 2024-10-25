module Interpreter (eval) where

import Expr (Expr(..))
import Error (Error(..))

import qualified Data.Map as Map

eval :: Map.Map String Double -> Expr -> Either Error Double
eval vars (Num n) = Right n

eval vars (Sqrt e) = 
    case eval vars e of
        Right x | x >= 0 -> Right (sqrt x)
                 | otherwise -> Left (NegativeSqrt e)
        Left err -> Left err

eval vars (Add a b) = (+) <$> eval vars a <*> eval vars b
eval vars (Sub a b) = (-) <$> eval vars a <*> eval vars b
eval vars (Mul a b) = (*) <$> eval vars a <*> eval vars b
eval vars (Div a b) = 
    case eval vars b of
        Right 0 -> Left (DivByZero a b)
        Right x -> (/ x) <$> eval vars a
        Left err -> Left err
eval vars (Pow a b) =
    case (eval vars a, eval vars b) of
        (Right base, Right exp)
            | base < 0 && exp < 1 -> Left (NegativeSqrt (Pow a b))
            | otherwise -> Right (base ** exp)
        (Left err, _) -> Left err
        (_, Left err) -> Left err

eval vars (Var x) = case Map.lookup x vars of
    Just value -> Right value
    Nothing    -> Left (UndefinedVar x)

eval vars (Let var expr body) = 
    case eval vars expr of
        Right value -> eval (Map.insert var value vars) body
        Left err -> Left err
