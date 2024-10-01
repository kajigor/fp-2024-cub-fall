module Main where

import Control.Monad (unless)
import Text.Printf (printf)
import Data.Either (either)

data Expr = Number Double
           | Sqrt Expr 
           | Plus Expr Expr 
           | Minus Expr Expr 
           | Mult Expr Expr 
           | Div Expr Expr 
           | Pow Expr Expr
  deriving Eq

instance Show Expr where
  show (Number x) = show x
  show (Sqrt e) = "sqrt(" ++ (show e) ++ ")"
  show (Plus e1 e2) = "(" ++ (show e1) ++ ") + (" ++ (show e2) ++ ")"
  show (Minus e1 e2) = "(" ++ (show e1) ++ ") - (" ++ (show e2) ++ ")"
  show (Mult e1 e2) = "(" ++ (show e1) ++ ") * (" ++ (show e2) ++ ")"
  show (Div e1 e2) = "(" ++ (show e1) ++ ") / (" ++ (show e2) ++ ")"
  show (Pow e1 e2) = "(" ++ (show e1) ++ ") ^ (" ++ (show e2) ++ ")"

data SimpleError = NegSqrt Double 
                | DivByZero
                | ZeroNonPositivePow Double
  deriving Eq
data Error = Error SimpleError Expr
  deriving Eq

instance Show Error where
  show (Error simpleError expr) = (errorMsg simpleError) ++
                 " (while evaluating the expression " ++ (show expr) ++ ")"
    where errorMsg (NegSqrt x) = "error: cannot take sqrt of negative number " ++ show x
          errorMsg DivByZero = "error: division by zero"
          errorMsg (ZeroNonPositivePow x) = "error: raising 0 to non-positive power " ++ show x

addContext :: Expr -> (Double -> Double -> Either SimpleError Double) -> Double -> Double -> Either Error Double
addContext expr f x y = either (\res -> Left $ Error res expr) Right (f x y)

evalBinary :: (Double -> Double -> Either Error Double) -> (Either Error Double) -> (Either Error Double) -> (Either Error Double)
evalBinary f _ (Left e) = Left e
evalBinary f (Left e) _ = Left e
evalBinary f (Right x) (Right y) = f x y

wrap :: (Double -> Double -> Double) -> Double -> Double -> Either Error Double
wrap f x y = Right (f x y)

safeDiv :: Double -> Double -> Either SimpleError Double
safeDiv x y | y == 0 = Left DivByZero
            | otherwise = Right (x / y)

safePow :: Double -> Double -> Either SimpleError Double
safePow x y | (x == 0 && y <= 0) = Left $ ZeroNonPositivePow y
            | otherwise = Right $ x ** y

safeSqrt :: Double -> Double -> Either SimpleError Double
safeSqrt x _ | x < 0 = Left $ NegSqrt x
             | otherwise = Right $ sqrt x

eval :: Expr -> Either Error Double
eval (Number x) = Right x
eval expr@(Sqrt x) = evalBinary (addContext expr safeSqrt) (eval x) (Right 0)
eval (Plus x y) = evalBinary (wrap (+)) (eval x) (eval y)
eval (Minus x y) = evalBinary (wrap (-)) (eval x) (eval y)
eval (Mult x y) = evalBinary (wrap (*)) (eval x) (eval y)
eval expr@(Div x y) = evalBinary (addContext expr safeDiv) (eval x) (eval y)
eval expr@(Pow x y) = evalBinary (addContext expr safePow) (eval x) (eval y)

cases :: [(Expr, Either Error Double)]
cases = [
  -- Simple expressions
  (Number 0.5, Right 0.5),
  (Sqrt (Number 25.0), Right 5.0),
  (Plus (Number 2.0) (Number 2.0), Right 4.0),
  (Minus (Number 2.0) (Number 4.0), Right (-2.0)),
  (Mult (Number 5.0) (Number 0.2), Right 1.0),
  (Div (Number 1.0) (Number 5.0), Right 0.2),
  (Pow (Number 6.0) (Number 4.0), Right 1296),

  -- Errors
  (Sqrt (Number (-1)), Left $ Error (NegSqrt (-1)) (Sqrt (Number (-1)))),
  (Div (Number 5) (Number 0), Left $ Error DivByZero (Div (Number 5) (Number 0))),
  (Pow (Number 0) (Number (-1)), Left $ Error (ZeroNonPositivePow (-1)) (Pow (Number 0) (Number (-1)))),
  (Pow (Number 0) (Number 0), Left $ Error (ZeroNonPositivePow 0) (Pow (Number 0) (Number 0))),

  -- Complex expression
  (Pow (Number 2) (Div (Plus (Number 74) (Number 6)) (Number 5)), Right 65536),

  -- Error in complex expression
  (Plus (Number 1) $ Div (Number 2) (Div (Number 0) (Number 1)), Left $ Error DivByZero (Div (Number 2) (Div (Number 0) (Number 1)))),

  -- Nested errors
  (Pow (Number 0) (Pow (Number 0) (Number 0)), Left $ Error (ZeroNonPositivePow 0) (Pow (Number 0) (Number 0))),

  -- Two errors on the same level
  (Plus (Sqrt (Number (-1))) (Div (Number 0) (Number 0)), Left $ Error DivByZero (Div (Number 0) (Number 0)))
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
