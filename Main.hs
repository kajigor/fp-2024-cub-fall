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
  show (Num a) = show a
  show (Sqrt a) = printf "sqrt(%s)" (show a)
  show (Add a b) = printf "(%s + %s)" (show a) (show b)
  show (Sub a b) = printf "(%s - %s)" (show a) (show b)
  show (Mul a b) = printf "(%s * %s)" (show a) (show b)
  show (Div a b) = printf "(%s / %s)" (show a) (show b)
  show (Pow a b) = printf "(%s ^ %s)" (show a) (show b)

instance Eq Expr where
  Num a == Num b = a == b
  Sqrt a == Sqrt b = a == b
  Add a b == Add c d = a == c && b == d
  Sub a b == Sub c d = a == c && b == d
  Mul a b == Mul c d = a == c && b == d
  Div a b == Div c d = a == c && b == d
  Pow a b == Pow c d = a == c && b == d
  _ == _ = False 

data Error = DivByZero Expr Expr
            | NegativeSqrt Expr

instance Show Error where
  show (DivByZero a b ) = printf "Cannot divide %s by %s" (show a) (show b)
  show (NegativeSqrt a ) = printf "Cannot take the square root of a negative number: %s" (show a)

instance Eq Error where
  DivByZero _ _ == DivByZero _ _ = True
  NegativeSqrt _ == NegativeSqrt _ = True
  _ == _ = False

evalBinary :: (Double -> Double -> Double) -> Expr -> Expr -> Either Error Double
evalBinary op x y = do
    ex <- eval x
    ey <- eval y
    return (ex `op` ey)

eval :: Expr -> Either Error Double
eval (Num x)     = Right x
eval (Add x y)   = evalBinary (+) x y
eval (Sub x y)   = evalBinary (-) x y
eval (Mul x y)   = evalBinary (*) x y
eval (Div x y)   = do
    ex <- eval x
    ey <- eval y
    if ey == 0
        then Left ( DivByZero x y )
        else return (ex / ey)
eval (Pow x y)   = evalBinary (**) x y
eval (Sqrt x)    = do
    ex <- eval x
    if ex < 0
        then Left ( NegativeSqrt x )
        else return (sqrt ex)

cases :: [(Expr, Either Error Double)]
cases = [   (Num 5, Right 5.0),
            (Add (Num 2) (Num 3), Right 5.0),
            (Sub (Num 10) (Num 7), Right 3.0),
            (Mul (Num 4) (Num 5), Right 20.0),
            (Div (Num 20) (Num 4), Right 5.0),
            (Div (Num 10) (Num 0), Left ( DivByZero (Num 10) (Num 0.0)) ),
            (Pow (Num 2) (Num 3), Right 8.0),
            (Sqrt (Num 9), Right 3.0),
            (Sqrt (Num (-4)), Left ( NegativeSqrt (Num (-4)) )),
            (Add (Mul (Num 2) (Num 3)) (Div (Num 10) (Num 2)), Right 11.0),
            (Sub (Pow (Num 2) (Num 3)) (Sqrt (Num 16)), Right 4.0),
            (Div (Mul (Num 2) (Num 5)) (Add (Num 4) (Num (-4))), Left ( DivByZero (Num 7) (Num 0) ))
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
