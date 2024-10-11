module Expr(Expr(..)) where
import Text.Printf (printf)

data Expr = 
  Number Double
  | Sqrt Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  | Exp Expr Expr
  | Var String
  | Let String Expr Expr
  deriving (Eq, Ord)
instance Show Expr where
  show (Number a) = show a
  show (Sqrt a) = printf "sqrt (%s)" (show a)
  show (Add a b) = printf "(%s + %s)" (show a) (show b)
  show (Sub a b) = printf "(%s - %s)" (show a) (show b)
  show (Mult a b) = printf "(%s * %s)" (show a) (show b)
  show (Div a b) = printf "(%s / %s)" (show a) (show b)
  show (Exp a b) = printf "(%s ^ %s)" (show a) (show b)
  show (Var var) = var
  show (Let var a b) = printf "let "var" = %s in %s" (show a) (show b)
  
data Error = 
  DividedByZero Expr
  | SqrtOfNegative Expr
  | NegativeExponent Expr
  deriving (Eq)

instance Show Error where
  show (DividedByZero expr) = printf "Any expression divided by 0 is undefined. In the expression: %s" (show expr)
  show (SqrtOfNegative expr) = printf "Taking a square root of a negative number is undefined. In the expression: %s" (show expr)
  show (NegativeExponent expr) = printf "Number raised to the power of a negative number is undefined. In the expression: %s" (show expr)
