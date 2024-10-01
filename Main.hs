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
  show (Num a) = printf "%s" (show a)
  show (Sqrt a) = printf ("Square root of %s") (show a) 
  show (Add a b) = printf ("%s + %s") (show a) (show b)
  show (Sub a b) = printf ("%s - %s") (show a) (show b)
  show (Mul a b) = printf ("%s * %s") (show a) (show b)
  show (Div a b) = printf ("%s / %s") (show a) (show b)
  show (Pow a b) = printf ("%s ^ %s") (show a) (show b)

instance Eq Expr where
  Num a == Num b = a == b
  Sqrt a == Sqrt b = a == b
  _ == _ = False 

data Error = DivByZero | NegativeSqrt

instance Show Error where
  show (DivByZero) = printf "Cannot divide by zero" 
  show (NegativeSqrt) = printf "Cannot take the square root of a negative number"

instance Eq Error where
  DivByZero == DivByZero = True
  NegativeSqrt == NegativeSqrt = True
  _ == _ = False

eval :: Expr -> Either Error Double
eval (Num x) = Right x
eval (Add x y) = do
    ex <- eval x
    ey <- eval y
    return (ex + ey)
eval (Sub x y) = do
    ex <- eval x
    ey <- eval y
    return (ex - ey)
eval (Mul x y) = do
    ex <- eval x
    ey <- eval y
    return (ex * ey)
eval (Div x y) = do
    ex <- eval x
    ey <- eval y
    if ey == 0
        then Left DivByZero
        else return (ex / ey)
eval (Pow x y) = do
    ex <- eval x
    ey <- eval y
    return (ex ** ey)
eval (Sqrt x) = do
    ex <- eval x
    if ex < 0
        then Left NegativeSqrt
        else return (sqrt ex)



cases :: [(Expr, Either Error Double)]
cases = [   ( Add (Num 2.0) (Num 3.0), Right 5.0 )
          , ( Sqrt (Num 16.0), Right 4.0 )
          , ( Sub (Num 10.0) (Num 7.0), Right 3.0 )
          , ( Div (Num 2.0) (Num 0.0), Left DivByZero )
          , ( Sqrt (Num (-16.0)), Left NegativeSqrt )
        ]


test :: Expr -> Either Error Double -> IO ()
test expr expected =
    let actual = eval expr in 
    unless (expected == actual) $ describeFailure actual
  where
    describeFailure actual =
      printf "eval (%s) should be %s but it was %s\n" (show expr) (show expected) (show actual)

main :: IO ()
main = do
  mapM_ (uncurry test) cases
  putStrLn "Done" 
