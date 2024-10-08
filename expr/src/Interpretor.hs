module Interpretor(main, eval) where

import Control.Monad (unless)
import Text.Printf (printf)
import qualified Data.Map as Map
import qualified Expr
import qualified Error

evalBinary :: (Double -> Double -> Double) -> Map.Map String Double -> Expr.Expr -> Expr.Expr -> Either Error.Error Double
evalBinary op cx a b = 
    case (eval cx a, eval cx b) of
        (Right aVal, Right bVal) -> Right (aVal `op` bVal)
        (Left err, _) -> Left err
        (_, Left err) -> Left err

eval :: Map.Map String Double -> Expr.Expr -> Either Error.Error Double
eval _ (Expr.Num x)     = Right x
eval cx (Expr.Add x y)   = evalBinary (+) cx x y
eval cx (Expr.Sub x y)   = evalBinary (-) cx x y
eval cx (Expr.Mul x y)   = evalBinary (*) cx x y
eval cx (Expr.Div x y)   = 
    case (eval cx x, eval cx y) of
        (Right xVal, Right yVal) 
            | yVal == 0 -> Left (Error.DivByZero x y)
            | otherwise -> Right (xVal / yVal)
        (Left err, _) -> Left err
        (_, Left err) -> Left err

eval cx (Expr.Pow x y)   = evalBinary (**) cx x y
eval cx (Expr.Sqrt x)    = do
    case eval cx x of
        Right xVal 
          | xVal >= 0 -> Right (sqrt xVal)
          | otherwise -> Left (Error.NegativeSqrt x)
        Left err -> Left err

eval cx (Expr.Var x) = 
    case Map.lookup x cx of
        Just value -> Right value
        Nothing -> Left (Error.UnassignedVar x)
    
eval cx (Expr.Let a b c) = 
    case eval cx b of
        Right aVal -> eval (Map.insert a aVal cx) c
        Left err -> Left err
    

cases :: [(Expr.Expr, Either Error.Error Double)]
cases = [   (Expr.Num 5, Right 5.0),
            (Expr.Add (Expr.Num 2) (Expr.Num 3), Right 5.0),
            (Expr.Sub (Expr.Num 10) (Expr.Num 7), Right 3.0),
            (Expr.Mul (Expr.Num 4) (Expr.Num 5), Right 20.0),
            (Expr.Div (Expr.Num 20) (Expr.Num 4), Right 5.0),
            (Expr.Div (Expr.Num 10) (Expr.Num 0), Left ( Error.DivByZero (Expr.Num 10) (Expr.Num 0.0)) ),
            (Expr.Pow (Expr.Num 2) (Expr.Num 3), Right 8.0),
            (Expr.Sqrt (Expr.Num 9), Right 3.0),
            (Expr.Sqrt (Expr.Num (-4)), Left ( Error.NegativeSqrt (Expr.Num (-4)) )),
            (Expr.Add (Expr.Mul (Expr.Num 2) (Expr.Num 3)) (Expr.Div (Expr.Num 10) (Expr.Num 2)), Right 11.0),
            (Expr.Sub (Expr.Pow (Expr.Num 2) (Expr.Num 3)) (Expr.Sqrt (Expr.Num 16)), Right 4.0),
            (Expr.Div (Expr.Mul (Expr.Num 2) (Expr.Num 5)) (Expr.Add (Expr.Num 4) (Expr.Num (-4))), Left ( Error.DivByZero (Expr.Num 7) (Expr.Num 0) ))
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