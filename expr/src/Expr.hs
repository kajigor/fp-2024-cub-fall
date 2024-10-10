module Expr (Expr(..)) where


data Expr
  = Let      String Expr Expr
  | Var      String
  | Number   Double
  | Sqrt     Expr
  | Add      Expr Expr
  | Subtract Expr Expr
  | Multiply Expr Expr
  | Divide   Expr Expr
  | Power    Expr Expr
  deriving (Eq)

instance Show Expr where
  show (Let x bound body) = "let " ++ x ++ " = " ++ show bound ++ " in " ++ show body
  show (Var x)            = x
  show (Number x)         = show x
  show (Sqrt x)           = "sqrt(" ++ show x ++ ")"
  show (Add x y)          = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (Subtract x y)     = "(" ++ show x ++ " - " ++ show y ++ ")"
  show (Multiply x y)     = "(" ++ show x ++ " * " ++ show y ++ ")"
  show (Divide x y)       = "(" ++ show x ++ " / " ++ show y ++ ")"
  show (Power x y)        = "(" ++ show x ++ " ^ " ++ show y ++ ")"
