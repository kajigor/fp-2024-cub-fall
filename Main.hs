module Main where

import Control.Monad (unless)
import Text.Printf (printf)

data Expr
  = NumExpr Double
  | SqrtExpr Expr
  | AddExpr Expr Expr
  | SubExpr Expr Expr
  | MulExpr Expr Expr
  | DivExpr Expr Expr
  | PowerExpr Expr Expr

instance Show Expr where
  show (NumExpr a) = show a
  show (SqrtExpr a) = "(" ++ show a ++ ")"
  show (AddExpr a b) = "(" ++ show a ++ "+" ++ show b ++ ")"
  show (SubExpr a b) = "(" ++ show a ++ "-" ++ show b ++ ")"
  show (MulExpr a b) = "(" ++ show a ++ "*" ++ show b ++ ")"
  show (DivExpr a b) = "(" ++ show a ++ "/" ++ show b ++ ")"
  show (PowerExpr a b) = "(" ++ show a ++ "^" ++ show b ++ ")"

instance Eq Expr where
  NumExpr a == NumExpr b = a == b
  SqrtExpr a == SqrtExpr b = a == b
  AddExpr a b == AddExpr c d = a==c && b==d
  SubExpr a b == SubExpr c d = a==c && b==d
  MulExpr a b == MulExpr c d = a==c && b==d
  DivExpr a b == DivExpr c d = a==c && b==d
  PowerExpr a b == PowerExpr c d = a==c && b==d
  _ == _ = False

data Error
  = DivByZero
  | NegativeSqrtNum
  | OtherError String 

instance Show Error where
  show DivByZero = "Division by zero error"
  show NegativeSqrtNum = "Cannot take square root of negative number"
  show (OtherError s) = "Error: " ++ s

instance Eq Error where
  DivByZero == DivByZero = True
  NegativeSqrtNum == NegativeSqrtNum = True
  OtherError s == OtherError t = s == t
  _ == _ = False

evalHelper :: (Double -> Double -> Double) -> Expr -> Expr -> Either Error Double
evalHelper op a b = 
    case (eval a, eval b) of
        (Right aVal, Right bVal) -> Right (aVal `op` bVal)
        (Left err, _) -> Left err
        (_, Left err) -> Left err

eval :: Expr -> Either Error Double
eval (NumExpr x) = Right x

eval (AddExpr a b) = evalHelper (+) a b

eval (SubExpr a b) = evalHelper (-) a b

eval (MulExpr a b) = evalHelper (*) a b

eval (PowerExpr a b) = evalHelper (**) a b

eval (DivExpr a b) = 
    case (eval a, eval b) of
        (Right a, Right b) 
            | b == 0 -> Left DivByZero
            | otherwise -> Right (a / b)
        (Left err, _) -> Left err
        (_, Left err) -> Left err

eval (SqrtExpr x) = 
    case eval x of
        Right x 
          | x >= 0 -> Right (sqrt x)
          | otherwise -> Left NegativeSqrtNum
        Left err -> Left err

cases :: [(Expr, Either Error Double)]
cases = 
  [ (NumExpr 5, Right 5)
  , (AddExpr (NumExpr 2) (NumExpr 4), Right 6)
  , (SubExpr (NumExpr 10) (NumExpr 4), Right 6)
  , (MulExpr (NumExpr 2) (NumExpr 5), Right 10)
  , (DivExpr (NumExpr 10) (NumExpr 2), Right 5)
  , (DivExpr (NumExpr 10) (NumExpr 0), Left DivByZero)
  , (SqrtExpr (NumExpr 9), Right 3)
  , (SqrtExpr (NumExpr (-9)), Left NegativeSqrtNum)
  , (PowerExpr (NumExpr 2) (NumExpr 3), Right 8)
  , (AddExpr (MulExpr (NumExpr 2) (NumExpr 3)) (NumExpr 4), Right 10)
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