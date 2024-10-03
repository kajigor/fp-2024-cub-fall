module Main where

import Control.Monad (unless)
import Text.Printf (printf)

data Expr
  = Number   Double
  | Sqrt     Expr
  | Add      Expr Expr
  | Subtract Expr Expr
  | Multiply Expr Expr
  | Divide   Expr Expr
  | Power    Expr Expr
  deriving (Eq)

instance Show Expr where
  show (Number x)     = show x
  show (Sqrt x)       = "sqrt(" ++ show x ++ ")"
  show (Add x y)      = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (Subtract x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
  show (Multiply x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
  show (Divide x y)   = "(" ++ show x ++ " / " ++ show y ++ ")"
  show (Power x y)    = "(" ++ show x ++ " ^ " ++ show y ++ ")"

data Error
  = NegativeSqrt Expr
  | DivisionByZero Expr
  deriving (Eq)

instance Show Error where
  show (NegativeSqrt expr)   = "Error: Square root of a negative number: \"" ++ show expr ++ "\""
  show (DivisionByZero expr) = "Error: Division by zero: \"" ++ show expr ++ "\""

evalBinary :: (Double -> Double -> Double) -> Expr -> Expr -> Either Error Double
evalBinary op x y = do
  valX <- eval x
  valY <- eval y
  return (valX `op` valY)

eval :: Expr -> Either Error Double
eval (Number x) = Right x
eval (Sqrt x) = case eval x of
  Left err -> Left err
  Right val
    | val < 0 -> Left (NegativeSqrt x)
    | otherwise -> Right (sqrt val)
eval (Divide x y) = do
  valY <- eval y
  case valY of
    0 -> Left (DivisionByZero y)
    _ -> evalBinary (/) x y
eval (Add x y)      = evalBinary (+)  x y
eval (Subtract x y) = evalBinary (-)  x y
eval (Multiply x y) = evalBinary (*)  x y
eval (Power x y)    = evalBinary (**) x y

cases :: [(Expr, Either Error Double)]
cases =
  [ (Number 4, Right 4)
  , (Sqrt     (Number 4),       Right 2)
  , (Sqrt     (Number 0.0),     Right 0.0)
  , (Sqrt     (Number (-2.25)), Left (NegativeSqrt (Number (-2.25))))

  , (Add      (Number 1.5) (Number 2.5),  Right 4.0)
  , (Subtract (Number 5)   (Number 3),    Right 2)
  , (Multiply (Number 3.0) (Number 2.0),  Right 6.0)
  , (Divide   (Number 5.0) (Number 2.5),  Right 2.0)
  , (Divide   (Number 6)   (Number 0),    Left (DivisionByZero (Divide (Number 6) (Number 0))))
  , (Power    (Number 2)   (Number 3),    Right 8)
  , (Power    (Number 2)   (Number (-1)), Right 0.5)
  , (Power    (Number 0)   (Number 0),    Right 1)
  ]

test :: Expr -> Either Error Double -> IO ()
test expr expected =
    let actual = eval expr in 
    unless (expected == actual) $ describeFailure actual
  where
    describeFailure actual =
      printf "eval (%s) should be %s but it was %s" (show expr) (show expected) (show actual)

main :: IO ()
main = do
  mapM_ (uncurry test) cases
  putStrLn "Done" 
