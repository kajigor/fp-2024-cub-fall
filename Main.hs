module Main where

import Control.Monad (unless)
import Data.Either (fromRight, isLeft)
import Text.Printf (printf)

data Expr
  = Numb Double
  | SqrtExpr Expr
  | BinOpAdd Expr Expr
  | BinOpSub Expr Expr
  | BinOpMul Expr Expr
  | BinOpDiv Expr Expr
  | BinOpPow Expr Expr
  deriving (Eq)

instance Show Expr where
  show (Numb a) = printf "%s" (show a)
  show (SqrtExpr a) = printf "sqrt %s" (show a)
  show (BinOpAdd a b) = printf "(%s + %s)" (show a) (show b)
  show (BinOpSub a b) = printf "(%s - %s)" (show a) (show b)
  show (BinOpMul a b) = printf "(%s * %s)" (show a) (show b)
  show (BinOpDiv a b) = printf "(%s / %s)" (show a) (show b)
  show (BinOpPow a b) = printf "(%s ^ %s)" (show a) (show b)

data Error = DivByZero Expr | SqrtNeg Expr
  deriving (Eq)

instance Show Error where
  show (DivByZero s) = printf "Expression evaluation failed in %s. Second arg for div cannot be zero\n" (show s)
  show (SqrtNeg s) = printf "Expression evaluation failed in %s. Arg for sqrt cannot be negative\n" (show s)


evalBinOp :: (Double -> Double -> Double) -> Expr -> Expr -> Either Error Double
evalBinOp op a b
  | isLeft aAfterEval = aAfterEval
  | isLeft bAfterEval = bAfterEval
  | otherwise = Right (aVal `op` bVal)
  where
    aAfterEval = eval a
    bAfterEval = eval b
    aVal = fromRight 0 aAfterEval
    bVal = fromRight 0 bAfterEval

eval :: Expr -> Either Error Double
eval (Numb a) = Right a
eval (SqrtExpr a)
  | isLeft aAfterEval = aAfterEval
  | aVal < 0 = Left (SqrtNeg (SqrtExpr a))
  | otherwise = Right (sqrt aVal)
  where
    aAfterEval = eval a
    aVal = fromRight 0 aAfterEval
eval (BinOpAdd a b) = evalBinOp (+) a b
eval (BinOpSub a b) = evalBinOp (-) a b
eval (BinOpMul a b) = evalBinOp (*) a b
eval (BinOpDiv a b) = case eval b of
  (Left x) -> Left x
  (Right x) -> if x==0 then Left (DivByZero (BinOpDiv a b)) else evalBinOp (/) a b
eval (BinOpPow a b) = evalBinOp (**) a b

cases :: [(Expr, Either Error Double)]
cases =
  [ (Numb 5, Right 5),
    (BinOpAdd (Numb 2) (Numb 3), Right 5),
    (BinOpSub (Numb 5) (Numb 3), Right 2),
    (BinOpMul (Numb 2) (Numb 4), Right 8),
    (BinOpDiv (Numb 10) (Numb 2), Right 5),
    (BinOpDiv (Numb 10) (Numb 0), Left (DivByZero (BinOpDiv (Numb 10) (Numb 0)))),
    (BinOpPow (Numb 2) (Numb 3), Right 8),
    (SqrtExpr (Numb 4), Right 2),
    (SqrtExpr (Numb (-9)), Left (SqrtNeg (SqrtExpr (Numb (-9))))),
    ( BinOpDiv
        ( BinOpMul
            (BinOpAdd (Numb 2) (Numb 3))
            (SqrtExpr (Numb 16))
        )
        (Numb 2),
      Right 10
    )
  ]

test :: Expr -> Either Error Double -> IO ()
test expr expected =
  let actual = eval expr
   in unless (expected == actual) $ describeFailure actual
  where
    describeFailure actual =
      printf "eval (%s) should be %s but it was %s" (show expr) (show expected) (show actual)

main :: IO ()
main = do
  mapM_ (uncurry test) cases
  putStrLn "Done"
