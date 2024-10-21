module Expr 
  ( Expr(..)
  , showExpr
  ) where

data Expr
  = Num Double
  | Var String
  | Let String Expr Expr
  | Sqrt Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  deriving (Eq, Read)

instance Show Expr where
  show = showExpr

showExpr :: Expr -> String
showExpr (Num x) = show x
showExpr (Var s) = s
showExpr (Let var expr body) = "let " ++ var ++ " = " ++ showExpr expr ++ " in " ++ showExpr body
showExpr (Sqrt e) = "sqrt(" ++ showExpr e ++ ")"
showExpr (Add e1 e2) = "(" ++ showExpr e1 ++ " + " ++ showExpr e2 ++ ")"
showExpr (Sub e1 e2) = "(" ++ showExpr e1 ++ " - " ++ showExpr e2 ++ ")"
showExpr (Mul e1 e2) = "(" ++ showExpr e1 ++ " * " ++ showExpr e2 ++ ")"
showExpr (Div e1 e2) = "(" ++ showExpr e1 ++ " / " ++ showExpr e2 ++ ")"
showExpr (Pow e1 e2) = "(" ++ showExpr e1 ++ " ^ " ++ showExpr e2 ++ ")"