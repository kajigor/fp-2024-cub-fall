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
  | Variable String
  | Let String Expr Expr     
  deriving(Eq)        

instance Show Expr where
  show (Num a) = show a
  show (Sqrt a) = printf "sqrt(%s)" (show a)
  show (Add a b) = printf "(%s + %s)" (show a) (show b)
  show (Sub a b) = printf "(%s - %s)" (show a) (show b)
  show (Mul a b) = printf "(%s * %s)" (show a) (show b)
  show (Div a b) = printf "(%s / %s)" (show a) (show b)
  show (Pow a b) = printf "(%s ^ %s)" (show a) (show b)
  show (Variable a) = show a
  show (Let a b c) = printf "let %s = %s in %s" (show a) (show b) (show c)
