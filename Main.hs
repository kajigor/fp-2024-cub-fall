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

instance Show Expr where
  show (Num a) = show a
  show (Sqrt a) = show "sqrt(" ++ show a ++ ")" 
  show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (Sub x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
  show (Mul x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
  show (Div x y) = "(" ++ show x ++ " / " ++ show y ++ ")"
  show (Pow x y) = "(" ++ show x ++ " ^ " ++ show y ++ ")"

instance Eq Expr where
  (==) (Num a) (Num b) = a == b
  (==) (Sqrt a) (Sqrt b) = a == b
  (==) (Add a b) (Add c d) = (a == c) && (b == d)
  (==) (Sub a b) (Sub c d) = (a == c) && (b == d)
  (==) (Mul a b) (Mul c d) = (a == c) && (b == d)
  (==) (Div a b) (Div c d) = (a == c) && (b == d)
  (==) (Pow a b) (Pow c d) = (a == c) && (b == d)
  (==) _ _ = False


data Error = DivZero Expr | NegRoot Expr

instance Show Error where
  show (DivZero expr) = "division by zero is forbidden " ++ show expr
  show (NegRoot expr) = "sqrt of a negative number is forbidden " ++ show expr

instance Eq Error where
  (==) x y = show x == show y

evenRoots = [1/x | x<-[2,4..31]]  -- we can take more here but I think 1e-9 is pretty small enough

performOperation :: (Double -> Double -> Double) -> Expr -> Expr -> Either Error Double
performOperation func a b
  | isLeft x = x
  | isLeft y = y
  | otherwise = Right (func (fromRight 1 x) (fromRight 1 y))
  where 
    x = eval a
    y = eval b


eval :: Expr -> Either Error Double
eval (Num a) = Right a
eval (Sqrt a) = case eval a of
  (Left x) -> Left x
  (Right x) -> if x < 0 then Left (NegRoot (Sqrt a)) else Right (sqrt x) 
eval (Add a b) = performOperation (+) a b
eval (Sub a b) = performOperation (-) a b
eval (Mul a b) = performOperation (*) a b
eval (Div a b) = 
  if fromRight 1 (eval b) == 0 
    then Left (DivZero (Div a b)) 
  else 
    performOperation (/) a b  

-- I needed to handle myself negative roots because for negative bases since it uses logarithms for the calculations gives nan
eval (Pow a b) 
  | isLeft x = x
  | isLeft y = y  
  | fromRight 0 x < 0 && fromRight 0 y < 1 =
    if fromRight 0 y `elem` evenRoots
      then Left (NegRoot (Pow a b)) 
    else   --make it positive first then take the power and then multiply it again
      Right (negate (abs (fromRight 0 x) ** fromRight 0 y))
  | otherwise = performOperation (**) a b
  where 
    x = eval a
    y = eval b


{-  Why this doesn't work?, somehow for negative numbers and odd powers it gives NaN. It would be nice to have an 
explanation
eval (Pow a b) 
  | y `elem` evenRoots && x < 0 = Left (NegRoot (Pow a b))
  | otherwise = performOperation (**) a b
  where 
    x = fromRight 0 (eval a)
    y = fromRight 0 (eval b)
-}  
    
    

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
  
  , (Pow (Num (-8.0)) (Num (1/3)), Right (-2))  -- (-8)^(1/3) = -2, should work for odd roots
  
  , (Pow (Num (-8)) (Num (1/2)), Left (NegRoot (Pow (Num (-8)) (Num (1/2)))))  -- (-8)^(1/2) = error for even roots
  
  , (Add (Mul (Num 2) (Num 3)) (Div (Num 10) (Num 2)), Right 11)  -- (2 * 3) + (10 / 2) = 6 + 5 = 11
  
  , (Sub (Num 0) (Sqrt (Num (-4))), Left (NegRoot (Sqrt (Num (-4)))))  -- sqrt of a negative number
  
  , (Mul (Pow (Num (-4)) (Num (1/2))) (Num 2), Left (NegRoot (Pow (Num (-4)) (Num (1/2))))) -- sqrt of a negative number in a more complex expression
  
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
