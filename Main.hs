module Main where

import Control.Monad (unless)
import Text.Printf (printf)
import Data.Either

data Expr =
  Num Double
  | Sqrt Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr

x = Add (Num 4) (Mul (Num 4) (Num 6))
y = Sqrt (Num 4)
z = Add (Num 2) (Num 4)
instance Show Expr where
  show (Num a) = show a
  show (Sqrt a) = show "sqrt(" ++ show a ++ ")" 
  show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (Sub x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
  show (Mul x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
  show (Div x y) = "(" ++ show x ++ " / " ++ show y ++ ")"
  show (Pow x y) = "(" ++ show x ++ " ^ " ++ show y ++ ")"

instance Eq Expr where
  (==) x y = show x == show y

data Error = DivZero | SqrNeg

instance Show Error where
  show DivZero = "division by zero is forbidden"
  show SqrNeg = "sqrt of a negative number is forbidden"

instance Eq Error where
  (==) x y = show x == show y

isError :: Either Error Double -> Bool
isError (Left _) = True
isError (Right _) = False


eval :: Expr -> Either Error Double
eval (Num a) = Right a
eval (Add a b)
  | isError x = x
  | isError y = y
  | otherwise = Right (fromRight 0 x + fromRight 0 y)
  where
      x = eval a
      y = eval b
eval (Sub a b)
  | isError x = x
  | isError y = y
  | otherwise = Right (fromRight 0 x - fromRight 0 y)
  where
      x = eval a
      y = eval b
eval (Mul a b)
  | isError x = x
  | isError y = y
  | otherwise = Right (fromRight 1 x * fromRight 1 y)
  where
      x = eval a
      y = eval b
eval (Div a b)
  | isError x = x
  | isError y = y
  | fromRight 1 y == 0 = Left DivZero
  | otherwise = Right (fromRight 1 x / fromRight 1 y)
  where
      x = eval a
      y = eval b
eval (Sqrt a)
  | isError x = x
  | fromRight 0 x < 0 = Left SqrNeg
  | otherwise = Right (sqrt (fromRight 0 x))
  where
      x = eval a
eval (Pow a b)
  | isError x = x
  | isError y = y
  | otherwise = Right (fromRight 0 x ** fromRight 0 y)
  where
      x = eval a
      y = eval b


cases :: [(Expr, Either Error Double)]
cases = [ (Num 5, Right 5)  -- Simple case
  
  , (Add (Num 2) (Num 3), Right 5)  -- 2 + 3 = 5
  
  , (Sub (Num 10) (Num 4), Right 6)  -- 10 - 4 = 6
  
  , (Mul (Num 3) (Num 4), Right 12)  -- 3 * 4 = 12
  
  , (Div (Num 10) (Num 2), Right 5)  -- 10 / 2 = 5
  
  , (Div (Num 10) (Num 0), Left DivZero)  -- Division by 0, must give an error
  
  , (Pow (Num 2) (Num 3), Right 8)  -- 2 ^ 3 = 8
  
  , (Pow (Num 9) (Num 0.5), Right 3)  -- 9 ^ 0.5 = sqrt(9) = 3
  
  , (Sqrt (Num 16), Right 4)  -- sqrt(16) = 4
  
  , (Sqrt (Num (-9)), Left SqrNeg)  -- sqrt of a negative number must give an error
  
  , (Add (Mul (Num 2) (Num 3)) (Div (Num 10) (Num 2)), Right 11)  -- (2 * 3) + (10 / 2) = 6 + 5 = 11
  
  , (Sub (Num 0) (Sqrt (Num (-4))), Left SqrNeg)  -- sqrt of a negative number
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
