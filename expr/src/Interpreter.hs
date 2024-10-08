module Interpreter(main, eval) where

import Control.Monad (unless)
import Text.Printf (printf)
import qualified Data.Map as Map
import qualified Expr
import qualified Error

evalHelper :: (Double -> Double -> Double) -> Map.Map String Double -> Expr.Expr -> Expr.Expr -> Either Error.Error Double
evalHelper op ctx a b = 
    case (eval ctx a, eval ctx b) of
        (Right aVal, Right bVal) -> Right (aVal `op` bVal)
        (Left err, _) -> Left err
        (_, Left err) -> Left err

eval :: Map.Map String Double -> Expr.Expr -> Either Error.Error Double
eval _ (Expr.NumExpr x) = Right x

eval ctx (Expr.AddExpr a b) = evalHelper (+) ctx a b

eval ctx (Expr.SubExpr a b) = evalHelper (-) ctx a b

eval ctx (Expr.MulExpr a b) = evalHelper (*) ctx a b

eval ctx (Expr.PowerExpr a b) = evalHelper (**) ctx a b

eval ctx (Expr.DivExpr a b) = 
    case (eval ctx a, eval ctx b) of
        (Right aVal, Right bVal) 
            | bVal == 0 -> Left (Error.DivByZero a b)
            | otherwise -> Right (aVal / bVal)
        (Left err, _) -> Left err
        (_, Left err) -> Left err

eval ctx (Expr.SqrtExpr x) = 
    case eval ctx x of
        Right xVal 
          | xVal >= 0 -> Right (sqrt xVal)
          | otherwise -> Left (Error.NegativeSqrtNum x)
        Left err -> Left err

eval ctx (Expr.VarExpr x) = 
    case Map.lookup x ctx of
        Just value -> Right value
        Nothing -> Left (Error.UnassignedVar x)
    
eval ctx (Expr.Let a b c) = 
    case eval ctx b of
        Right aVal -> eval (Map.insert a aVal ctx) c
        Left err -> Left err
    

cases :: [(Expr.Expr, Either Error.Error Double)]
cases = 
  [ (Expr.NumExpr 5, Right 5)
  , (Expr.AddExpr (Expr.NumExpr 2) (Expr.NumExpr 4), Right 6)
  , (Expr.SubExpr (Expr.NumExpr 10) (Expr.NumExpr 4), Right 6)
  , (Expr.MulExpr (Expr.NumExpr 2) (Expr.NumExpr 5), Right 10)
  , (Expr.DivExpr (Expr.NumExpr 10) (Expr.NumExpr 2), Right 5)
  , (Expr.DivExpr (Expr.NumExpr 10) (Expr.NumExpr 0), Left (Error.DivByZero (Expr.NumExpr 10) (Expr.NumExpr 0)))
  , (Expr.SqrtExpr (Expr.NumExpr 9), Right 3)
  , (Expr.SqrtExpr (Expr.NumExpr (-9)), Left (Error.NegativeSqrtNum (Expr.NumExpr (-9))))
  , (Expr.PowerExpr (Expr.NumExpr 2) (Expr.NumExpr 3), Right 8)
  , (Expr.AddExpr (Expr.MulExpr (Expr.NumExpr 2) (Expr.NumExpr 3)) (Expr.NumExpr 4), Right 10)
  , (Expr.DivExpr (Expr.NumExpr 10) (Expr.SubExpr (Expr.NumExpr 1) (Expr.NumExpr 1)), Left (Error.DivByZero (Expr.NumExpr 10) (Expr.SubExpr (Expr.NumExpr 1) (Expr.NumExpr 1))))
  ]

test :: Expr.Expr -> Either Error.Error Double -> IO ()
test expr expected =
    let actual = eval Map.empty expr in 
    unless (expected == actual) $ describeFailure actual
  where
    describeFailure actual =
      printf "eval (%s) should be %s but it was %s\n" (show expr) (show expected) (show actual)

main :: IO ()
main = do
  mapM_ (uncurry test) cases
  putStrLn "Done"