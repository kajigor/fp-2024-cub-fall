module Expr (Expr (..)) where

data Expr =
  Num Double
  | Sqrt Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  | Var String
  | Let Expr Expr Expr
  deriving Eq

instance Show Expr where
  show (Num a) = show a
  show (Sqrt a) = show "sqrt(" ++ show a ++ ")" 
  show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (Sub x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
  show (Mul x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
  show (Div x y) = "(" ++ show x ++ " / " ++ show y ++ ")"
  show (Pow x y) = "(" ++ show x ++ " ^ " ++ show y ++ ")"
  show (Var name) = name
  show (Let (Var name) value body) = "let " ++ name ++ "=" ++ show value ++ " in " ++ show body
  show Let{} = undefined
