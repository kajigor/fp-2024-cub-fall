module Main where

import Control.Monad (unless)
import Text.Printf (printf)

data Expr = Num Double
          | Sqrt Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Pow Expr Expr

instance Show Expr where
  show (Num x) = show x
  show (Sqrt e) = "sqrt(" ++ show e ++ ")"
  show (Add e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
  show (Sub e1 e2) = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
  show (Mul e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
  show (Div e1 e2) = "(" ++ show e1 ++ " / " ++ show e2 ++ ")"
  show (Pow e1 e2) = "(" ++ show e1 ++ " ^ " ++ show e2 ++ ")"

instance Eq Expr where
  (Num x) == (Num y) = x == y
  (Sqrt e1) == (Sqrt e2) = e1 == e2
  (Add e1 e2) == (Add e3 e4) = e1 == e3 && e2 == e4
  (Sub e1 e2) == (Sub e3 e4) = e1 == e3 && e2 == e4
  (Mul e1 e2) == (Mul e3 e4) = e1 == e3 && e2 == e4
  (Div e1 e2) == (Div e3 e4) = e1 == e3 && e2 == e4
  (Pow e1 e2) == (Pow e3 e4) = e1 == e3 && e2 == e4
  _ == _ = False

data Error = DivisionByZero
           | NegativeSqrt
           deriving (Show, Eq)

eval :: Expr -> Either Error Double
eval (Num x) = Right x
eval (Sqrt e) = case eval e of
                  Right v -> if v >= 0 then Right (sqrt v) else Left NegativeSqrt
                  Left err -> Left err
eval (Add e1 e2) = evalBinOp (+) e1 e2
eval (Sub e1 e2) = evalBinOp (-) e1 e2
eval (Mul e1 e2) = evalBinOp (*) e1 e2
eval (Div e1 e2) = case eval e2 of
                     Right 0 -> Left DivisionByZero
                     Right v2 -> evalBinOp (/) e1 (Num v2)
                     Left err -> Left err
eval (Pow e1 e2) = evalBinOp (**) e1 e2

evalBinOp :: (Double -> Double -> Double) -> Expr -> Expr -> Either Error Double
evalBinOp op e1 e2 = case (eval e1, eval e2) of
                       (Right v1, Right v2) -> Right (v1 `op` v2)
                       (Left err, _) -> Left err
                       (_, Left err) -> Left err

cases :: [(Expr, Either Error Double)]
cases = [ (Num 5, Right 5),
          (Add (Num 3) (Num 4), Right 7),
          (Sub (Num 10) (Num 5), Right 5),
          (Mul (Num 2) (Num 3), Right 6),
          (Div (Num 10) (Num 2), Right 5),
          (Div (Num 1) (Num 0), Left DivisionByZero),
          (Pow (Num 2) (Num 3), Right 8),
          (Sqrt (Num 16), Right 4),
          (Sqrt (Num (-16)), Left NegativeSqrt)
        ]

test :: Expr -> Either Error Double -> IO ()
test expr expected =
    let actual = eval expr in 
    unless (expected == actual) $ describeFailure actual
  where
    describeFailure actual =
      printf "eval (%s) should be %s but it was %s\n" (show expr) (show expected) (show actual)

main :: IO ()
main = do
  mapM_ (uncurry test) cases
  putStrLn "Done"