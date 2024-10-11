module Interpreter(eval) where
import Expr
import Error
import qualified Data.Map.Strict as M
evalBinary :: M.Map String Double -> Expr -> Expr -> (Double -> Double -> b) -> Either Error b
evalBinary state a b op = case (eval state a, eval state b) of
  (Right numA, Right numB) -> Right (numA `op` numB)
  (Left errorA, _) -> Left errorA
  (_, Left errorB) -> Left errorB

eval :: M.Map String Double -> Expr -> Either Error Double
eval _ (Number a) = Right a
eval state (Sqrt a) = case eval state a of
  Right num
    | num < 0 -> Left (SqrtOfNegative (Sqrt a))
    | otherwise -> Right (sqrt num)
  Left err -> Left err

eval state (Add a b) = evalBinary state a b (+)
eval state (Sub a b) = evalBinary state a b (-)
eval state (Mult a b) = evalBinary state a b (*)

eval state (Div a b) = case (eval state a, eval state b) of
  (Right numA, Right numB)
    | numB == 0 -> Left (DividedByZero (Div a b))
    | otherwise -> Right (numA / numB)
  (Left errorA, _) -> Left errorA
  (_, Left errorB) -> Left errorB
  
eval state (Exp a b) = case (eval state a, eval state b) of
  (Right numA, Right numB)
    | numB < 0 -> Left (NegativeExponent (Exp a b))
    | otherwise -> Right (numA ** numB)
  (Left errorA, _) -> Left errorA
  (_, Left errorB) -> Left errorB

eval state (Var var) = case M.lookup var state of
  Just num -> Right num
  Nothing -> Left (NotVariable var)

eval state (Let var a b) = case M.lookup var state of --Let "x" (Number 13) (Add (Var "x") (Number 1))
  Just _ -> Left (MultipleDeclaration var)            
  Nothing -> case eval state a of                     -- eval (state = M.empty) (Number 13) -> Right 13
    Right num -> eval (M.insert var num state) b      -- Right 13 -> eval (state = M.insert "x" 13) (Add (Var "x") (Number 1)) -> Right 
    Left err -> Left err
