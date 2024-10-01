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
  (==) (Number a) (Number b) = a == b
  (==) (Sqrt a) (Sqrt b) = a == b
  (==) (Plus a b) (Plus c d) = a == c && b == d
  (==) (Minus a b) (Minus c d) = a == c && b == d
  (==) (Prod a b) (Prod c d) = a == c && b == d
  (==) (Divide a b) (Divide c d) = a == c && b == d
  (==) (Exp a b) (Exp c d) = a == c && b == d
  (==) _ _ = False

data Error
  = DivisionByZero Expr
  | RootOfNegative Expr
  | ZeroToNegativePower Expr
  deriving (Eq)

instance Show Error where
  show e = "ERROR! " ++ case e of
    DivisionByZero expr -> "Cannot divide by zero in \"" ++ show expr ++ "\""
    RootOfNegative expr -> "Cannot take a root of a negative number in \"" ++ show expr ++ "\""
    ZeroToNegativePower expr -> "Cannot raise 0 to a negative power in \"" ++ show expr ++ "\""

evalBinaryOperands :: Expr -> Expr -> (Double -> Double -> Double)-> Either Error Double
evalBinaryOperands a b op =
  case eval a of
    Left err -> Left err
    Right eval_a ->
      case eval b of
        Left err -> Left err
        Right eval_b -> Right (op eval_a eval_b)

eval :: Expr -> Either Error Double
eval (Number x)= Right x
eval expr@(Sqrt x) =
  case eval x of
  Left err -> Left err
  Right eval_x
      | eval_x >= 0 -> Right (sqrt eval_x)
      | otherwise -> Left (RootOfNegative expr)
eval (Plus a b) = evalBinaryOperands a b (+)
eval (Minus a b) = evalBinaryOperands a b (-)
eval (Prod a b) = evalBinaryOperands a b (*)
eval expr@(Divide a b) =
  case evalBinaryOperands a b (/) of 
    Left err -> Left err
    Right res
      | isInfinite res -> Left (DivisionByZero expr)
      | otherwise -> Right res
eval expr@(Exp a b) =
  case evalBinaryOperands a b (**) of 
    Left err -> Left err
    Right res
      | isNaN res -> Left (RootOfNegative expr)
      | isInfinite res -> Left (ZeroToNegativePower expr)
      | otherwise -> Right res

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
  (testNegativeRoot, Left (RootOfNegative testNegativeRoot)),
  (testZeroToNegative, Left (ZeroToNegativePower testZeroToNegative))
  ] 
  where
    testNegativeSquareRoot = Sqrt (Prod (Number (-3)) (Number 44))
    testDivideByZero = Divide (Number 5) (Minus (Number 4) (Number 4))
    testNegativeRoot = Exp (Number (-1)) (Divide (Number 1) (Number 2))
    testZeroToNegative = Exp (Number 0) (Minus (Number 0) (Number 10))

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
