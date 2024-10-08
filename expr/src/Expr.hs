module Expr (Expr (..)) where

import Text.Printf (printf)

data Expr
  = Num Double
  | Sqrt Expr
  | Minus Expr Expr
  | Plus Expr Expr
  | Multi Expr Expr
  | Div Expr Expr
  | Power Expr Expr
  | Var String
  | Let String Expr Expr

instance Show Expr where
  show (Num a) = show a
  show (Sqrt a) = printf "(sqrt %s)" (show a)
  show (Minus a b) = printf "(%s - %s)" (show a) (show b)
  show (Plus a b) = printf "(%s + %s)" (show a) (show b)
  show (Multi a b) = printf "(%s * %s)" (show a) (show b)
  show (Div a b) = printf "(%s / %s)" (show a) (show b)
  show (Power a b) = printf "(%s ^ %s)" (show a) (show b)
  show (Var a) = show a
  show (Let a b c) = printf "let %s = %s in %s" (show a) (show b) (show c)

instance Eq Expr where
  Num a == Num b = a == b
  Sqrt a == Sqrt b = a == b
  Minus a b == Minus c d = a == c && b == d
  Plus a b == Plus c d = a == c && b == d
  Multi a b == Multi c d = a == c && b == d
  Div a b == Div c d = a == c && b == d
  Power a b == Power c d = a == c && b == d
  Var a == Var b = a == b
  Let a b c == Let d e f = a == d && b == e && c == f
  _ == _ = False