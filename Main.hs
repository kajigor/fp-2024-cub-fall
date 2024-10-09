
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
  deriving (Show, Eq)

data Error
  = DivByZero Expr
  | NegativeSqrt Expr
  | InvalidPow Expr
  | OtherError String
  deriving (Eq)

instance Show Error where
  show (DivByZero expr) = "Division by zero in: " ++ show expr
  show (NegativeSqrt expr) = "Square root of a negative number in: " ++ show expr
  show (InvalidPow expr) = "Invalid power operation in: " ++ show expr
  show (OtherError msg) = "Error: " ++ msg

eval :: Expr -> Either Error Double
eval (Num x) = Right x
eval (Sqrt e) = do
  x <- eval e
  if x < 0
    then Left (NegativeSqrt (Sqrt e))
    else Right (sqrt x)
eval (Add e1 e2) = (+) <$> eval e1 <*> eval e2
eval (Sub e1 e2) = (-) <$> eval e1 <*> eval e2
eval (Mul e1 e2) = (*) <$> eval e1 <*> eval e2
eval (Div e1 e2) = do
  x <- eval e2
  if x == 0
    then Left (DivByZero (Div e1 e2))
    else (/) <$> eval e1 <*> Right x
eval (Pow e1 e2) = do
  base <- eval e1
  exp <- eval e2
  if base < 0 && exp /= fromInteger (round exp)
    then Left (InvalidPow (Pow e1 e2))
    else Right (base ** exp)

cases :: [(Expr, Either Error Double)]
cases =
  [ (Num 4, Right 4)
  , (Sqrt (Num 4), Right 2)
  , (Sqrt (Num (-4)), Left (NegativeSqrt (Sqrt (Num (-4)))))
  , (Add (Num 1) (Num 2), Right 3)
  , (Sub (Num 5) (Num 3), Right 2)
  , (Mul (Num 2) (Num 3), Right 6)
  , (Div (Num 6) (Num 2), Right 3)
  , (Div (Num 4) (Num 0), Left (DivByZero (Div (Num 4) (Num 0))))
  , (Pow (Num 2) (Num 3), Right 8)
  , (Pow (Num (-2)) (Num 0.5), Left (InvalidPow (Pow (Num (-2)) (Num 0.5))))
  , (Div (Mul (Add (Num 4) (Num 5)) (Sub (Sqrt (Num 16)) (Num 2))) (Num 2), Right 9)
  , (Pow (Num 2) (Num 10), Right 1024)
  , (Sub (Num 3) (Num 5), Right (-2))
  , (Pow (Num (-8)) (Div (Num 1) (Num 3)), Left (InvalidPow (Pow (Num (-8)) (Div (Num 1) (Num 3)))))
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
