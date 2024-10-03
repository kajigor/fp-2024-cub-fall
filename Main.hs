module Main where

import Control.Monad (unless)
import Text.Printf (printf)
import Language.Haskell.TH (Exp, safe)
import Data.Either (isLeft, fromRight)
import Control.Arrow (ArrowChoice(right))

data Expr = Numb Double
      | SqrtExpr Expr
      | BinOpAdd Expr Expr
      | BinOpSub Expr Expr
      | BinOpMul Expr Expr
      | BinOpDiv Expr Expr
      | BinOpPow Expr Expr

instance Show Expr where
  show (Numb a) = printf "%s" (show a)
  show (SqrtExpr a) = printf "sqrt %s" (show a)
  show (BinOpAdd a b) = printf "(%s + %s)" (show a) (show b)
  show (BinOpSub a b) = printf "(%s - %s)" (show a) (show b)
  show (BinOpMul a b) = printf "(%s * %s)" (show a) (show b)
  show (BinOpDiv a b) = printf "(%s / %s)" (show a) (show b)
  show (BinOpPow a b) = printf "(%s ^ %s)" (show a) (show b)

instance Eq Expr where
  Numb a == Numb b = a == b
  SqrtExpr a == SqrtExpr b = a == b
  BinOpAdd a b == BinOpAdd c d = (a == c) && (b == d)
  BinOpSub a b == BinOpSub c d = (a == c) && (b == d)
  BinOpMul a b == BinOpMul c d = (a == c) && (b == d)
  BinOpDiv a b == BinOpDiv c d = (a == c) && (b == d)
  BinOpPow a b == BinOpPow c d = (a == c) && (b == d)
  _ == _ = False

data Error = DivByZero String | SqrtNeg String

instance Show Error where
  show (DivByZero s) = printf "%s" (show s)
  show (SqrtNeg s) = show s

instance Eq Error where
  DivByZero s1 == DivByZero s2 = s1 == s2
  SqrtNeg s1 == SqrtNeg s2 = s1 == s2
  _ == _ = False

eval :: Expr -> Either Error Double
eval (Numb a) = Right a
eval (SqrtExpr a)
    | isLeft aAfterEval = aAfterEval
    | aVal < 0 = Left (SqrtNeg "Arg for sqrt cannot be negative")
    | otherwise = Right (sqrt aVal )
    where aAfterEval = eval a
          aVal = fromRight 0 aAfterEval
eval (BinOpAdd a b)
    | isLeft aAfterEval = aAfterEval
    | isLeft bAfterEval = bAfterEval
    | otherwise = Right (aVal + bVal)
    where aAfterEval = eval a
          bAfterEval = eval b
          aVal = fromRight 0 aAfterEval
          bVal = fromRight 0 bAfterEval
eval (BinOpSub a b)
  | isLeft aAfterEval = aAfterEval
  | isLeft bAfterEval = bAfterEval
  | otherwise = Right (aVal - bVal)
  where
    aAfterEval = eval a
    bAfterEval = eval b
    aVal = fromRight 0 aAfterEval
    bVal = fromRight 0 bAfterEval
eval (BinOpMul a b)
  | isLeft aAfterEval = aAfterEval
  | isLeft bAfterEval = bAfterEval
  | otherwise = Right (aVal * bVal)
  where
    aAfterEval = eval a
    bAfterEval = eval b
    aVal = fromRight 0 aAfterEval
    bVal = fromRight 0 bAfterEval
eval (BinOpDiv a b)
  | isLeft aAfterEval = aAfterEval
  | isLeft bAfterEval = bAfterEval
  | bVal == 0 = Left (DivByZero "Second arg for div cannot be zero")
  | otherwise = Right (aVal / bVal)
  where
    aAfterEval = eval a
    bAfterEval = eval b
    aVal = fromRight 0 aAfterEval
    bVal = fromRight 0 bAfterEval
eval (BinOpPow a b)
  | isLeft aAfterEval = aAfterEval
  | isLeft bAfterEval = bAfterEval
  | otherwise = Right (aVal ** bVal)
  where
    aAfterEval = eval a
    bAfterEval = eval b
    aVal = fromRight 0 aAfterEval
    bVal = fromRight 0 bAfterEval

cases :: [(Expr, Either Error Double)]
cases =
  [
    (Numb 5, Right 5),

    (BinOpAdd (Numb 2) (Numb 3), Right 5),

    (BinOpSub (Numb 5) (Numb 3), Right 2),

    (BinOpMul (Numb 2) (Numb 4), Right 8),

    (BinOpDiv (Numb 10) (Numb 2), Right 5),

    (BinOpDiv (Numb 10) (Numb 0), Left (DivByZero "Second arg for div cannot be zero")),

    (BinOpPow (Numb 2) (Numb 3), Right 8),

    (SqrtExpr (Numb 4), Right 2),

    (SqrtExpr (Numb (-9)), Left (SqrtNeg "Arg for sqrt cannot be negative")),

    (BinOpDiv 
        (BinOpMul 
            (BinOpAdd (Numb 2) (Numb 3)) 
            (SqrtExpr (Numb 16))) 
        (Numb 2), 
     Right 10)
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
