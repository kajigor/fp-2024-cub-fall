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


instance Show Expr where
  show (Num x) = show x
  show (Sqrt e) = "sqrt(" ++ show e ++ ")"
  show (Add e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
  show (Sub e1 e2) = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
  show (Mul e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
  show (Div e1 e2) = "(" ++ show e1 ++ " / " ++ show e2 ++ ")"
  show (Pow e1 e2) = "(" ++ show e1 ++ " ^ " ++ show e2 ++ ")"

instance Eq Expr where
  (Num x)==(Num y)=x==y
  (Sqrt e1)==(Sqrt e2)=e1==e2
  (Add e1 e2)==(Add e3 e4)=e1==e3 && e2==e4
  (Sub e1 e2)==(Sub e3 e4)=e1==e3 && e2==e4
  (Mul e1 e2)==(Mul e3 e4)=e1==e3 && e2==e4
  (Div e1 e2)==(Div e3 e4)=e1==e3 && e2==e4
  (Pow e1 e2)==(Pow e3 e4)=e1==e3 && e2==e4
  _ == _ = False

data Error
  = NegativeSqrt
  | DivisionByZero

instance Show Error where
  show NegativeSqrt = "Invalid: Negative square root"
  show DivisionByZero = "Invalid: Division by zero"

instance Eq Error where
  NegativeSqrt==NegativeSqrt=True
  DivisionByZero==DivisionByZero=True
  _ == _ = False


eval :: Expr -> Either Error Double
eval (Num x) = Right x
eval (Sqrt e) = case eval e of
  Left err-> Left err
  Right x-> if x < 0 then Left NegativeSqrt else Right (sqrt x)
eval (Add e1 e2) = do
  x <-eval e1
  y <-eval e2
  return (x+y)
eval (Sub e1 e2) = do
  x <-eval e1
  y <-eval e2
  return (x-y)
eval (Mul e1 e2) = do
  x <-eval e1
  y <-eval e2
  return (x*y)
eval (Div e1 e2) = do
  x <-eval e1
  y <-eval e2
  if y == 0 then Left DivisionByZero else Right (x/y)
eval (Pow e1 e2) = do
  x <-eval e1
  y <-eval e2
  return (x**y)

cases :: [(Expr, Either Error Double)]
cases =
  [ (Num 5, Right 5)
  , (Add (Num 2) (Num 3), Right 5)
  , (Sub (Num 5) (Num 3), Right 2)
  , (Mul (Num 4) (Num 3), Right 12)
  , (Div (Num 10) (Num 2), Right 5)
  , (Pow (Num 2) (Num 3), Right 8)
  , (Sqrt (Num 9), Right 3)
  , (Sqrt (Num (-1)), Left NegativeSqrt)
  , (Div (Num 1) (Num 0), Left DivisionByZero)
  , (Mul (Div (Num 4) (Num 2)) (Sqrt (Num 16)), Right 8)
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
