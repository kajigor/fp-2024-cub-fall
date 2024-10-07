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
  deriving Eq

instance Show Expr where
  show (Num a) = show a
  show (Sqrt a) = show "sqrt(" ++ show a ++ ")" 
  show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (Sub x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
  show (Mul x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
  show (Div x y) = "(" ++ show x ++ " / " ++ show y ++ ")"
  show (Pow x y) = "(" ++ show x ++ " ^ " ++ show y ++ ")"

data Error = DivZero Expr | NegRoot Expr deriving Eq

instance Show Error where
  show (DivZero expr) = "division by zero is forbidden " ++ show expr
  show (NegRoot expr) = "sqrt of a negative number is forbidden " ++ show expr


performOperation :: (Double -> Double -> Double) -> Expr -> Expr -> Either Error Double
performOperation func a b = case (eval a, eval b) of
  (Left a, _) -> Left a -- Not using fromRight here
  (_, Left a) -> Left a
  (Right a, Right b) -> Right (func a b)


eval :: Expr -> Either Error Double
eval (Num a) = Right a
eval (Sqrt a) = case eval a of
  (Left x) -> Left x
  (Right x) -> if x < 0 then Left (NegRoot (Sqrt a)) else Right (sqrt x) 
eval (Add a b) = performOperation (+) a b
eval (Sub a b) = performOperation (-) a b
eval (Mul a b) = performOperation (*) a b
eval (Div a b) = case eval b of
  Right 0 -> Left (DivZero (Div a b)) -- Not using fromRight
  _ -> performOperation (/) a b
eval (Pow a b) = performOperation (**) a b   -- deleted the cases with negatives 
                                -- bases since they are not required in the task 

cases :: [(Expr, Either Error Double)]
cases = [ (Num 5, Right 5)  -- Simple case
  
  , (Add (Num 2) (Num 3), Right 5)  -- 2 + 3 = 5
  
  , (Sub (Num 10) (Num 4), Right 6)  -- 10 - 4 = 6
  
  , (Mul (Num 3) (Num 4), Right 12)  -- 3 * 4 = 12
  
  , (Div (Num 10) (Num 2), Right 5)  -- 10 / 2 = 5
  
  , (Div (Num 10) (Num 0), Left (DivZero (Div (Num 10) (Num 0))))  -- Division by 0, must give an error
  
  , (Pow (Num 2) (Num 3), Right 8)  -- 2 ^ 3 = 8
  
  , (Pow (Num 9.0) (Num 0.5), Right 3)  -- 9 ^ 0.5 = sqrt(9) = 3
  
  , (Sqrt (Num 16), Right 4)  -- sqrt(16) = 4
  
  , (Sqrt (Num (-9)), Left (NegRoot (Sqrt (Num (-9)))))  -- sqrt of a negative number must give an error

  , (Add (Mul (Num 2) (Num 3)) (Div (Num 10) (Num 2)), Right 11)  -- (2 * 3) + (10 / 2) = 6 + 5 = 11
  
  , (Sub (Num 0) (Sqrt (Num (-4))), Left (NegRoot (Sqrt (Num (-4)))))  -- sqrt of a negative number
    
  , (Pow (Num 16) (Num (1/4)), Right 2)  -- 16^(1/4) = 2
  
  , (Pow (Num 27) (Num (1/3)), Right 3)  -- 27^(1/3) = 3, cube root
  
  , (Div (Pow (Num 16) (Num (1/2))) (Num 0), Left (DivZero (Div (Pow (Num 16) (Num (1/2))) (Num 0)))) -- sqrt(16) / 0 should return division by zero error
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
