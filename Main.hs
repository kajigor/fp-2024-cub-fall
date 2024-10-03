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
    deriving(Eq)

instance Show Expr where
    show(Num a) = show a
    show(Sqrt a) = printf "sqrt(%s)" (show a)
    show(Add a b) = printf "%s + %s" (show a) (show b)
    show(Sub a b) = printf "%s - %s" (show a) (show b)
    show(Mul a b) = printf "%s * %s" (show a) (show b)
    show(Div a b) = printf "%s / %s" (show a) (show b)
    show(Pow a b) = printf "%s ^ %s" (show a) (show b)


data Error = SqrtProblem Expr
    |DivisionProblem Expr
    deriving(Eq)

instance Show Error where
  show(SqrtProblem expr) = "Error: Square root of negative number: " ++ show expr ++ "."
  show(DivisionProblem expr) = "Eroor: division by zero: " ++ show expr ++ "."

binaryExpr :: (Double -> Double -> Double) -> Expr -> Expr -> Either Error Double
binaryExpr op a b = do
    an <- eval a
    bn <- eval b
    return (an `op` bn)

eval :: Expr -> Either Error Double
eval (Num a) = Right a
eval (Add a b)      = binaryExpr (+)  a b
eval (Sub a b) = binaryExpr (-)  a b
eval (Mul a b) = binaryExpr (*)  a b
eval (Pow a b)    = binaryExpr (**) a b
eval (Sqrt a) = case eval a of
    Left val -> Left val
    Right val
        | val < 0 -> Left(SqrtProblem a)
        | otherwise -> Right(sqrt val)
eval (Div a b) = do
    an <- eval a
    bn <- eval b
    case bn of 
        0 -> Left(DivisionProblem(Div a b))
        _ -> binaryExpr (/) a b


cases :: [(Expr, Either Error Double)]
cases = 
    [(Num 13, Right 13), (Add (Num 13) (Num (-5)), Right 8)
    , (Sqrt (Num 4), Right 2), (Sqrt (Num (-2)), Left(SqrtProblem( Num (-2))))
    , (Sub (Num 16) (Num 20), Right (-4)), (Mul (Num 5) (Num 2), Right 10)
    , (Pow (Num 2) (Num 4), Right 16), (Div (Num 2) (Num 4), Right 0.5)
    , (Div (Num 14) (Num 0), Left(DivisionProblem(Div (Num 14) (Num 0))))
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
