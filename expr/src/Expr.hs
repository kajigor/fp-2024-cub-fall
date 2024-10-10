module Expr (Expr(..)) where

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
  deriving (Eq)

instance Show Expr where
  show (Num n) = show n
  show (Sqrt e) = "sqrt(" ++ show e ++ ")"
  show (Add a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
  show (Sub a b) = "(" ++ show a ++ " - " ++ show b ++ ")"
  show (Mul a b) = "(" ++ show a ++ " * " ++ show b ++ ")"
  show (Div a b) = "(" ++ show a ++ " / " ++ show b ++ ")"
  show (Pow a b) = "(" ++ show a ++ " ^ " ++ show b ++ ")"
  show (Var x) = x
  show (Let var expr body) = "let " ++ var ++ " = " ++ show expr ++ " in " ++ show body
