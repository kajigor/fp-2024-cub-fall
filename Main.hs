{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Monad (unless)
import Text.Printf (printf)

data Expr
  = Num Double
  | Sqrt Expr
  | Minus Expr Expr
  | Plus Expr Expr
  | Multi Expr Expr
  | Div Expr Expr
  | Power Expr Expr

instance Show Expr where
  show (Num a) = show a
  show (Sqrt a) = printf "sqrt %s" (show a)
  show (Minus a b) = printf "%s - %s" (show a) (show b)
  show (Plus a b) = printf "%s + %s" (show a) (show b)
  show (Multi a b) = printf "%s" (show a) (show b)
  show (Div a b) = printf "%s / %s" (show a) (show b)
  show (Power a b) = printf "%s ^ %s" (show a) (show b)

instance Eq Expr where
  (==) :: Expr -> Expr -> Bool
  Num a == Num b = a == b
  Sqrt a == Sqrt b = a == b
  Minus a b == Minus c d = a == c && b == d
  Plus a b == Plus c d = a == c && b == d
  Multi a b == Multi c d = a == c && b == d
  Div a b == Div c d = a == c && b == d
  Power a b == Power c d = a == c && b == d
  _ == _ = False

data Error
  = DivisionByZero Expr Expr
  | SqrtOfNegNumber Expr

instance Show Error where
  show (DivisionByZero a b) = printf "Error: tried to do devision by zero: %s / %s" (show a) (show b)
  show (SqrtOfNegNumber a) = printf "Error: tried to find the square root of a negative number: sqrt %s" (show a)

instance Eq Error where
  DivisionByZero _ _ == DivisionByZero _ _ = True
  SqrtOfNegNumber _ == SqrtOfNegNumber _ = True
  _ == _ = False

eval :: Expr -> Either Error Double
eval (Num a) = Right a
eval (Sqrt a) = do
  val <- eval a
  if val >= 0 then Right (sqrt val) else Left (SqrtOfNegNumber a)
eval (Minus a b) = do
  val1 <- eval a
  val2 <- eval b
  Right (val1 - val2)
eval (Plus a b) = do
  val1 <- eval a
  val2 <- eval b
  Right (val1 + val2)
eval (Multi a b) = do
  val1 <- eval a
  val2 <- eval b
  Right (val1 * val2)
eval (Div a b) = do
  val1 <- eval a
  val2 <- eval b
  if val2 == 0
    then Left (DivisionByZero a b)
    else Right (val1 / val2)
eval (Power a b) = do
  val1 <- eval a
  val2 <- eval b
  Right (val1 ** val2)

cases :: [(Expr, Either Error Double)]
cases =
  [ (Num 10, Right 10.0),
    (Num (-1), Right (-1.0)),
    (Sqrt (Num 25), Right 5.0),
    (Sqrt (Num (-1)), Left (SqrtOfNegNumber (Num (-1)))),
    (Minus (Num 78) (Num 8), Right 70.0),
    (Plus (Num 67) (Num 13), Right 80.0),
    (Multi (Plus (Num 78) (Num 13)) (Num 0), Right 0.0),
    (Multi (Plus (Num 10) (Num 15)) (Num 4), Right 100.0),
    (Div (Num 0) (Num 87), Right 0.0),
    (Div (Num 1) (Num 2), Right 0.5),
    (Div (Num 34) (Num 0), Left (DivisionByZero (Num 34) (Num 0))),
    (Power (Num 3) (Num 2), Right 9.0),
    (Power (Num 98) (Num 0), Right 1.0)
  ]

test :: Expr -> Either Error Double -> IO ()
test expr expected =
  let actual = eval expr
   in unless (expected == actual) $ describeFailure actual
  where
    describeFailure actual =
      printf "eval (%s) should be %s but it was %s" (show expr) (show expected) (show actual)

main :: IO ()
main = do
  mapM_ (uncurry test) cases
  putStrLn "Done"
