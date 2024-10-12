module Expr (Expr(..)) where

data Expr
  = Number Double
  | Sqrt Expr
  | Plus Expr Expr
  | Minus Expr Expr
  | Prod Expr Expr
  | Divide Expr Expr
  | Exp Expr Expr
  | Variable String 
  | Let String Expr Expr
  deriving Eq

instance Show Expr where
  show (Number x) = show x
  show (Sqrt x) = "sqrt(" ++ show x ++ ")"
  show (Plus a b) = "(" ++ show a ++ ") + (" ++ show b ++ ")"
  show (Minus a b) = "(" ++ show a ++ ") - (" ++ show b ++ ")"
  show (Prod a b) = "(" ++ show a ++ ") * (" ++ show b ++ ")"
  show (Divide a b) = "(" ++ show a ++ ") / (" ++ show b ++ ")"
  show (Exp a b) = "(" ++ show a ++ ") ^ (" ++ show b ++ ")"
  show (Variable name) = name
  show (Let name eq_value in_expr) = "let " ++ name ++ " = " ++ show eq_value ++ " in " ++ show in_expr
  