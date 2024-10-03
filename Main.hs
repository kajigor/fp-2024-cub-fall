module Main where

import Control.Monad (unless)
import Text.Printf (printf)

data Expr
  = Number Double
  | SquareRoot Expr
  | Add Expr Expr
  | Subtract Expr Expr
  | Multiply Expr Expr
  | Divide Expr Expr
  | Power Expr Expr
  deriving (Eq)

instance Show Expr where
  show (Number n) = show n
  show (SquareRoot e) = "√(" ++ show e ++ ")"
  show (Add e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
  show (Subtract e1 e2) = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
  show (Multiply e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
  show (Divide e1 e2) = "(" ++ show e1 ++ " / " ++ show e2 ++ ")"
  show (Power e1 e2) = "(" ++ show e1 ++ " ^ " ++ show e2 ++ ")"

data Error
  = DivisionByZero
  | NegativeSqrt
  deriving (Eq, Show)

eval :: Expr -> Either Error Double
eval (Number n) = Right n
eval (SquareRoot e) =
  case eval e of
    Right x | x < 0 -> Left NegativeSqrt
            | otherwise -> Right (sqrt x)
    Left err -> Left err
eval (Add e1 e2) = do
  x <- eval e1
  y <- eval e2
  return (x + y)
eval (Subtract e1 e2) = do
  x <- eval e1
  y <- eval e2
  return (x - y)
eval (Multiply e1 e2) = do
  x <- eval e1
  y <- eval e2
  return (x * y)
eval (Divide e1 e2) = do
  x <- eval e1
  y <- eval e2
  if y == 0
    then Left DivisionByZero
    else return (x / y)
eval (Power e1 e2) = do
  x <- eval e1
  y <- eval e2
  return (x ** y)

cases :: [(Expr, Either Error Double)]
cases =
  [ (Number 5, Right 5)
  , (Add (Number 3) (Number 2), Right 5)
  , (Subtract (Number 5) (Number 3), Right 2)
  , (Multiply (Number 4) (Number 2), Right 8)
  , (Divide (Number 6) (Number 2), Right 3)
  , (Divide (Number 6) (Number 0), Left DivisionByZero)
  , (SquareRoot (Number 9), Right 3)
  , (SquareRoot (Number (-4)), Left NegativeSqrt)
  , (Power (Number 2) (Number 3), Right 8)
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
