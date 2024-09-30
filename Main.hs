module Main where

import Control.Monad (unless)
import Text.Printf (printf)

data Expr
  = Number Double
  | Sqrt Expr
  | Plus Expr Expr
  | Minus Expr Expr
  | Prod Expr Expr
  | Divide Expr Expr
  | Exp Expr Expr

instance Show Expr where
  show (Number x) = show x
  show (Sqrt x) = "sqrt(" ++ show x ++ ")"
  show (Plus a b) = "(" ++ show a ++ ") + (" ++ show b ++ ")"
  show (Minus a b) = "(" ++ show a ++ ") - (" ++ show b ++ ")"
  show (Prod a b) = "(" ++ show a ++ ") * (" ++ show b ++ ")"
  show (Divide a b) = "(" ++ show a ++ ") / (" ++ show b ++ ")"
  show (Exp a b) = "(" ++ show a ++ ") ^ (" ++ show b ++ ")"


instance Eq Expr where
  (==) :: Expr -> Expr -> Bool
  (==) a b = show a == show b

data Error
  = DivisionByZero Expr
  | RootOfNegative Expr

instance Show Error where
  show e = "ERROR! " ++ case e of
    DivisionByZero expr -> "Cannot divide by zero in \"" ++ show expr ++ "\""
    RootOfNegative expr -> "Cannot take a root of a negative number in \"" ++ show expr ++ "\""

instance Eq Error where
  (==) (DivisionByZero exp1) (DivisionByZero exp2) = exp1 == exp2
  (==) (RootOfNegative exp1) (RootOfNegative exp2) = exp1 == exp2
  (==) _ _ = False

evalBinaryOperands :: Expr -> Expr -> Either Error (Double, Double)
evalBinaryOperands a b =
  case eval a of
    Left err -> Left err
    Right eval_a ->
      case eval b of
        Left err -> Left err
        Right eval_b -> Right (eval_a, eval_b)


eval :: Expr -> Either Error Double
eval (Number x)= Right x
eval expr@(Sqrt x) =
  case eval x of
  Left err -> Left err
  Right eval_x ->
    if eval_x >= 0
      then Right (sqrt eval_x)
      else Left (RootOfNegative expr)
eval (Plus a b) =
  case evalBinaryOperands a b of 
    Left err -> Left err
    Right (left, right) -> Right (left + right)
eval (Minus a b) =
  case evalBinaryOperands a b of 
    Left err -> Left err
    Right (left, right) -> Right (left - right)
eval (Prod a b) =
  case evalBinaryOperands a b of 
    Left err -> Left err
    Right (left, right) -> Right (left * right)
eval expr@(Divide a b) =
  case evalBinaryOperands a b of 
    Left err -> Left err
    Right (left, right) -> 
      if right == 0 
        then Left (DivisionByZero expr) 
        else Right (left / right)
eval expr@(Exp a b) =
  case evalBinaryOperands a b of 
    Left err -> Left err
    Right (left, right) -> 
      let isWholeNumber x = x == fromIntegral (round x) in
      if left == 0 && right < 0 
        then Left (DivisionByZero expr) 
      else if left < 0 && not (isWholeNumber right)
        then Left (RootOfNegative expr)
        else Right (left ** right)

cases :: [(Expr, Either Error Double)]
cases = [
  -- Test every expression type
  (Number 3.0, Right 3.0),
  (Sqrt (Number 25.0), Right 5.0),
  (Plus (Number 1.0) (Number 10.0), Right 11.0),
  (Minus (Number 2.3) (Number 4.8), Right (-2.5)),
  (Prod (Number 4.0) (Number 2.5), Right 10.0),
  (Divide (Number 1.0) (Number 4.0), Right 0.25),
  (Exp (Number 2.0) (Number 10), Right 1024),

  -- Test composite expression 
  (Divide (Plus (Number 3.4) (Number 6.6)) (Sqrt (Number 25)), Right 2.0),
  (Plus (Prod (Number 2.0) (Sqrt (Number 9.0))) (Minus (Number 10.0) (Number 5.0)), Right 11.0),

  -- Test errors
  (testNegativeSquareRoot, Left (RootOfNegative testNegativeSquareRoot)),
  (testDivideByZero, Left (DivisionByZero testDivideByZero)),
  (testNegativeRoot, Left (RootOfNegative testNegativeRoot))
  ] 
  where
    testNegativeSquareRoot = Sqrt (Prod (Number (-3)) (Number 44))
    testDivideByZero = Divide (Number 5) (Minus (Number 4) (Number 4))
    testNegativeRoot = Exp (Number (-1)) (Divide (Number 1) (Number 2))

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
