module Main where

import Control.Monad (unless)
import Text.Printf (printf)

data Operator = Add 
     | Sub 
     | Mult 
     | Div 
     | Pow
     deriving (Eq)
     
instance Show Operator where
     show Add = "+"
     show Sub = "-"
     show Mult = "*"
     show Div = "/"
     show Pow = "^"

data Expr = Num Double 
     | Sqrt Expr 
     | CompExpr Operator Expr Expr
     deriving (Eq)

instance Show Expr where
    show (Num a) = show a
    show (Sqrt a) = printf "sqrt(%s)" (show a)
    show (CompExpr op a b) = printf "(%s %s %s)" (show a) (show op) (show b)
    
data Error = NegativeSqrt Expr
     | ZeroDiv Expr

instance Show Error where
  show (NegativeSqrt expr) = printf "Error: Square root of a negative number in expression: %s" (show expr)
  show (ZeroDiv expr) = printf "Error: Division by 0 is not possible in expression: %s" (show expr)

instance Eq Error where
  NegativeSqrt expr1 == NegativeSqrt expr2 = expr1 == expr2
  ZeroDiv expr1 == ZeroDiv expr2 = expr1 == expr2
  _ == _ = False

eval :: Expr -> Either Error Double
eval (Num x) = Right x
eval (Sqrt expr) =
   case eval expr of
      Right x
         | x >= 0 -> Right (sqrt x)
         | otherwise -> Left (NegativeSqrt expr)
      Left err -> Left err
eval (CompExpr op a b) = 
   case (eval a, eval b) of
      (Right val1, Right val2) -> 
         case op of
            Add  -> Right (val1 + val2)
            Sub  -> Right (val1 - val2)
            Mult -> Right (val1 * val2)
            Div  -> 
               if val2 == 0 
               then Left (ZeroDiv (CompExpr Div a b))
               else Right (val1 / val2)
            Pow ->
               if val1 == 0 && val2 < 0
               then Left (ZeroDiv (CompExpr Pow a b))
               else if val1 < 0 && (val2 /= fromInteger (round val2))
               then Left (NegativeSqrt (CompExpr Pow a b))
               else Right (val1 ** val2)
      (Left err, _) -> Left err
      (_, Left err) -> Left err                         

cases :: [(Expr, Either Error Double)]
cases = 
  [ (Num 3, Right 3.0)
  , (Sqrt (Num 9), Right 3.0)
  , (Sqrt (Num (-9)), Left (NegativeSqrt (Num (-9))))
  , (CompExpr Add (Num 3) (Num 7), Right 10.0)
  , (CompExpr Sub (Num 7) (Num 3), Right 4.0)
  , (CompExpr Sub (Num 3) (Num 7), Right (-4.0))
  , (CompExpr Mult (Num 2) (Num 10), Right 20.0)
  , (CompExpr Div (Num 9) (Num 3), Right 3.0)
  , (CompExpr Div (Num 3) (Num 0), Left (ZeroDiv (CompExpr Div (Num 3) (Num 0))))
  , (CompExpr Pow (Num 3) (Num 3), Right 27.0)
  , (Sqrt (CompExpr Sub (Num 7) (Num 3)), Right 2.0)
  , (Sqrt (CompExpr Sub (Num 3) (Num 7)), Left (NegativeSqrt (CompExpr Sub (Num 3) (Num 7))))
  , (CompExpr Add (CompExpr Mult (Num 2) (Num 10)) (CompExpr Sub (Num 10) (Num 2)), Right 28.0) 
  , (CompExpr Div (CompExpr Mult (Num 2) (Num 10)) (CompExpr Sub (Num 10) (Num 10)), Left (ZeroDiv (CompExpr Div (CompExpr Mult (Num 2) (Num 10)) (CompExpr Sub (Num 10) (Num 10)))))
  , (CompExpr Pow (Num 0) (Num (-4)), Left (ZeroDiv (CompExpr Pow (Num 0) (Num (-4)))))
  , (CompExpr Pow (Num (-10)) (Num 0.5), Left (NegativeSqrt (CompExpr Pow (Num (-10)) (Num 0.5))))
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
  putStrLn("Done")
