module Main where

import Control.Monad (unless)
import Text.Printf (printf)

data Expr
  = Num Double
  | Sqrt Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  deriving (Eq)

instance Show Expr where
  show (Num x) = show x
  show (Sqrt e) = "sqrt" ++ show e
  show (Add e1 e2) = show e1 ++ " + " ++ show e2
  show (Sub e1 e2) = show e1 ++ " - " ++ show e2
  show (Mul e1 e2) = show e1 ++ " * " ++ show e2
  show (Div e1 e2) = show e1 ++ " / " ++ show e2
  show (Pow e1 e2) = show e1 ++ " ^ " ++ show e2

data Error = 
  NegativeSqrt Expr |
  DivisionByZero Expr
  deriving (Eq)

instance Show Error where
  show (NegativeSqrt e) = "Invalid: Negative square root in expression: " ++ show e
  show (DivisionByZero e) = "Invalid: Division by zero in expression: " ++ show e

eval :: Expr -> Either Error Double
eval e@(Num x) = Right x
eval e@(Sqrt expr) = do
  x <- eval expr
  if x < 0 
    then Left (NegativeSqrt e)
    else Right (sqrt x)
eval e@(Div e1 e2) = do
  x <- eval e1
  y <- eval e2
  if y == 0 
    then Left (DivisionByZero e)
    else Right (x / y)
eval expr = do
  let (op, e1, e2) = case expr of
        Add a b -> ((+), a, b)
        Sub a b -> ((-), a, b)
        Mul a b -> ((*), a, b)
        Pow a b -> ((**), a, b)
  x <- eval e1
  y <- eval e2
  return (op x y)

cases :: [(Expr, Either Error Double)]
cases =
  [ (Num 5, Right 5)
  , (Add (Num 2) (Num 3), Right 5)
  , (Sub (Num 5) (Num 3), Right 2)
  , (Mul (Num 4) (Num 3), Right 12)
  , (Div (Num 10) (Num 2), Right 5)
  , (Pow (Num 2) (Num 3), Right 8)
  , (Sqrt (Num 9), Right 3)
  , (Sqrt (Num (-1)), Left (NegativeSqrt (Sqrt (Num (-1)))))
  , (Div (Num 1) (Num 0), Left (DivisionByZero (Div (Num 1) (Num 0))))
  , (Mul (Div (Num 4) (Num 2)) (Sqrt (Num 16)), Right 8)
  ]

test :: Expr -> Either Error Double -> IO ()
test expr expected =
    let actual = eval expr in 
    unless (expected == actual) $ describeFailure actual
  where
    describeFailure actual =
      printf "eval %s should be %s but it was %s" (show expr) (show expected) (show actual)

main :: IO ()
main = do
  mapM_ (uncurry test) cases
  putStrLn "Done"