module Interpreter(Interpreter(..)) where

import Control.Monad (unless)
import qualified Data.Map.Strict as M
import Text.Printf (printf)

import Expr
import Error

eval :: M.Map String Double -> Expr -> Either Error Double
eval _ (Num x) = Right x
eval m (Sqrt expr) =
   case eval m expr of
      Right x
         | x >= 0 -> Right (sqrt x)
         | otherwise -> Left (NegativeSqrt expr)
      Left err -> Left err
eval m (CompExpr op a b) = 
   case (eval m a, eval m b) of
      (Right val1, Right val2) -> 
         case op of
            Operator.Add  -> Right (val1 + val2)
            Operator.Sub  -> Right (val1 - val2)
            Operator.Mult -> Right (val1 * val2)
            Operator.Div  -> 
               if val2 == 0 
               then Left (ZeroDiv (CompExpr Operator.Div a b))
               else Right (val1 / val2)
            Operator.Pow ->
               if val1 == 0 && val2 < 0
               then Left (ZeroDiv (CompExpr Operator.Pow a b))
               else if val1 < 0 && (val2 /= fromInteger (round val2))
               then Left (NegativeSqrt (CompExpr Operator.Pow a b))
               else Right (val1 ** val2)
      (Left err, _) -> Left err
      (_, Left err) -> Left err
eval m (Var x) = 
   case M.lookup x m of
       Just assigned -> Right assigned
       Nothing -> Left (Unbounded x)
eval m (Let var expr exprBody) =
   case eval m expr of
      Right assigned -> eval (M.insert var assigned m) exprBody
      Left err -> Left err
