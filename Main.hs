module Main where

import Control.Monad (unless)
import Text.Printf (printf)

data Expr = 
  Number Double
  | Sqrt Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  | Exp Expr Expr
  deriving (Eq, Ord)
instance Show Expr where
  show (Number a) = show a
  show (Sqrt a) = printf "sqrt (%s)" (show a)
  show (Add a b) = printf "(%s + %s)" (show a) (show b)
  show (Sub a b) = printf "(%s - %s)" (show a) (show b)
  show (Mult a b) = printf "(%s * %s)" (show a) (show b)
  show (Div a b) = printf "(%s / %s)" (show a) (show b)
  show (Exp a b) = printf "(%s ^ %s)" (show a) (show b)
  
data Error = 
  DividedByZero Expr
  | SqrtOfNegative Expr
  | NegativeExponent Expr
  deriving (Eq)

instance Show Error where
  show (DividedByZero expr) = printf "Any expression divided by 0 is undefined. In the expression: %s" (show expr)
  show (SqrtOfNegative expr) = printf "Taking a square root of a negative number is undefined. In the expression: %s" (show expr)
  show (NegativeExponent expr) = printf "Number raised to the power of a negative number is undefined. In the expression: %s" (show expr)

evalBinary a b op = case (eval a, eval b) of
  (Right numA, Right numB) -> Right (numA `op` numB)
  (Left errorA, _) -> Left errorA
  (_, Left errorB) -> Left errorB

eval :: Expr -> Either Error Double
eval (Number a) = Right a
eval (Sqrt a) = case eval a of
  Right num
    | num < 0 -> Left (SqrtOfNegative (Sqrt a))
    | otherwise -> Right (sqrt num)
  Left error -> Left error

eval (Add a b) = evalBinary a b (+)
eval (Sub a b) = evalBinary a b (-)
eval (Mult a b) = evalBinary a b (*)

eval (Div a b) = case (eval a, eval b) of
  (Right numA, Right numB)
    | numB == 0 -> Left (DividedByZero (Div a b))
    | otherwise -> Right (numA / numB)
  (Left errorA, _) -> Left errorA
  (_, Left errorB) -> Left errorB
  
eval (Exp a b) = case (eval a, eval b) of
  (Right numA, Right numB)
    | numB < 0 -> Left (NegativeExponent (Exp a b))
    | otherwise -> Right (numA ** numB)
  (Left errorA, _) -> Left errorA
  (_, Left errorB) -> Left errorB

cases :: [(Expr, Either Error Double)]
cases = [ (Number 5, Right 5)
  , (Sqrt (Number 4), Right 2)
  , (Sqrt (Number (-4)), Left (SqrtOfNegative (Sqrt (Number (-4)))))
  , (Add (Number 3) (Number 2), Right 5)
  , (Sub (Number 10) (Number 4), Right 6)
  , (Mult (Number 3) (Number 4), Right 12)
  , (Div (Number 10) (Number 2), Right 5)
  , (Div (Number 10) (Number 0), Left (DividedByZero (Div (Number 10) (Number 0))))
  , (Exp (Number 2) (Number 3), Right 8)
  , (Exp (Number 2) (Number (-3)), Left (NegativeExponent (Exp (Number 2) (Number (-3)))))
  , (Div (Number 1) (Add (Number 1) (Number (-1))), Left (DividedByZero (Div (Number 1) (Add (Number 1) (Number (-1))))))
  , (Sqrt (Add (Number 3) (Number (-3))), Right 0)
  , (Mult (Sqrt (Number 4)) (Add (Number 2) (Number 2)), Right 8)
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
