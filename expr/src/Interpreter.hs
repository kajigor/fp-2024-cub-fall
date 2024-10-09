module Interpreter (main, eval) where

import Control.Monad (unless)
import qualified Data.Map as Map
import qualified Error
import qualified Expr
import Text.Printf (printf)

helper :: (Double -> Double -> Either Error.Error Double) -> Map.Map String Double -> Expr.Expr -> Expr.Expr -> Either Error.Error Double
helper op mp a b = do
  val1 <- eval mp a
  val2 <- eval mp b
  op val1 val2

eval :: Map.Map String Double -> Expr.Expr -> Either Error.Error Double
eval mp (Expr.Num a) = Right a
eval mp (Expr.Var a) =
  case Map.lookup a mp of
    Just a -> Right a
    Nothing -> Left (Error.UnassignedVar a)
eval mp (Expr.Sqrt a) = do
  val <- eval mp a
  if val >= 0 then Right (sqrt val) else Left (Error.SqrtOfNegNumber a)
eval mp (Expr.Minus a b) = helper (\x y -> Right (x - y)) mp a b
eval mp (Expr.Plus a b) = helper (\x y -> Right (x + y)) mp a b
eval mp (Expr.Multi a b) = helper (\x y -> Right (x * y)) mp a b
eval mp (Expr.Div a b) =
  case (eval mp a, eval mp b) of
    (Right val1, Right val2)
      | val2 == 0 -> Left (Error.DivisionByZero a b)
      | otherwise -> Right (val1 / val2)
    (Left err, _) -> Left err
    (_, Left err) -> Left err
eval mp (Expr.Power a b) = helper (\x y -> Right (x ** y)) mp a b
eval mp (Expr.Let a b c) =
  case eval mp b of
    Right val1 -> eval (Map.insert a val1 mp) c
    Left err -> Left err

cases :: [(Expr.Expr, Either Error.Error Double)]
cases =
  [ (Expr.Num 10, Right 10.0),
    (Expr.Num (-1), Right (-1.0)),
    (Expr.Sqrt (Expr.Num 25), Right 5.0),
    (Expr.Sqrt (Expr.Num (-1)), Left (Error.SqrtOfNegNumber (Expr.Num (-1)))),
    (Expr.Minus (Expr.Num 78) (Expr.Num 8), Right 70.0),
    (Expr.Plus (Expr.Num 67) (Expr.Num 13), Right 80.0),
    (Expr.Multi (Expr.Plus (Expr.Num 78) (Expr.Num 13)) (Expr.Num 0), Right 0.0),
    (Expr.Multi (Expr.Plus (Expr.Num 10) (Expr.Num 15)) (Expr.Num 4), Right 100.0),
    (Expr.Div (Expr.Num 0) (Expr.Num 87), Right 0.0),
    (Expr.Div (Expr.Num 1) (Expr.Num 2), Right 0.5),
    (Expr.Div (Expr.Num 34) (Expr.Num 0), Left (Error.DivisionByZero (Expr.Num 34) (Expr.Num 0))),
    (Expr.Power (Expr.Num 3) (Expr.Num 2), Right 9.0),
    (Expr.Power (Expr.Num 98) (Expr.Num 0), Right 1.0)
  ]

test :: Expr.Expr -> Either Error.Error Double -> IO ()
test expr expected =
  let actual = eval Map.empty expr
   in unless (expected == actual) $ describeFailure actual
  where
    describeFailure actual =
      printf "eval (%s) should be %s but it was %s" (show expr) (show expected) (show actual)

main :: IO ()
main = do
  mapM_ (uncurry test) cases
  putStrLn "Done"