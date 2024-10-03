
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
  (Num x) == (Num y) = x == y
  (Sqrt e1) == (Sqrt e2) = e1 == e2
  (Add e1 e2) == (Add f1 f2) = e1 == f1 && e2 == f2
  (Sub e1 e2) == (Sub f1 f2) = e1 == f1 && e2 == f2
  (Mul e1 e2) == (Mul f1 f2) = e1 == f1 && e2 == f2
  (Div e1 e2) == (Div f1 f2) = e1 == f1 && e2 == f2
  (Pow e1 e2) == (Pow f1 f2) = e1 == f1 && e2 == f2
  _ == _ = False

data Error
  = DivByZero
  | NegativeSqrt
  | OtherError String

instance Show Error where
  show DivByZero = "Division by zero"
  show NegativeSqrt = "Square root of a negative number"
  show (OtherError msg) = "Error: " ++ msg

instance Eq Error where
  DivByZero == DivByZero = True
  NegativeSqrt == NegativeSqrt = True
  (OtherError msg1) == (OtherError msg2) = msg1 == msg2
  _ == _ = False

eval :: Expr -> Either Error Double
eval (Num x) = Right x
eval (Sqrt e) = do
  x <- eval e
  if x < 0
    then Left NegativeSqrt
    else Right (sqrt x)
eval (Add e1 e2) = (+) <$> eval e1 <*> eval e2
eval (Sub e1 e2) = (-) <$> eval e1 <*> eval e2
eval (Mul e1 e2) = (*) <$> eval e1 <*> eval e2
eval (Div e1 e2) = do
  x <- eval e2
  if x == 0
    then Left DivByZero
    else (/) <$> eval e1 <*> Right x
eval (Pow e1 e2) = (**) <$> eval e1 <*> eval e2

cases :: [(Expr, Either Error Double)]
cases =
  [ (Num 4, Right 4)
  , (Sqrt (Num 4), Right 2)
  , (Sqrt (Num (-4)), Left NegativeSqrt)
  , (Add (Num 1) (Num 2), Right 3)
  , (Div (Num 4) (Num 0), Left DivByZero)
  , (Pow (Num 2) (Num 3), Right 8)
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
