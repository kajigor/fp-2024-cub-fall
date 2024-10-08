module Expr (Expr(..)) where
import Text.Printf (printf)

data Expr 
  = Num Double
  | Sqrt Expr 
  | Add Expr Expr       
  | Sub Expr Expr       
  | Mul Expr Expr       
  | Div Expr Expr       
  | Pow Expr Expr   
  | Var String
  | Let String Expr Expr             

instance Show Expr where
  show (Num a) = show a
  show (Sqrt a) = printf "sqrt(%s)" (show a)
  show (Add a b) = printf "(%s + %s)" (show a) (show b)
  show (Sub a b) = printf "(%s - %s)" (show a) (show b)
  show (Mul a b) = printf "(%s * %s)" (show a) (show b)
  show (Div a b) = printf "(%s / %s)" (show a) (show b)
  show (Pow a b) = printf "(%s ^ %s)" (show a) (show b)
  show (Var a) = show a
  show (Let a b c) = printf "let %s = %s in %s" (show a) (show b) (show c)

instance Eq Expr where
  Num a == Num b = a == b
  Sqrt a == Sqrt b = a == b
  Add a b == Add c d = a == c && b == d
  Sub a b == Sub c d = a == c && b == d
  Mul a b == Mul c d = a == c && b == d
  Div a b == Div c d = a == c && b == d
  Pow a b == Pow c d = a == c && b == d
  Var a == Var b = a == b
  Let a b c == Let d e f = a == d && b == e && c == f
  _ == _ = False 