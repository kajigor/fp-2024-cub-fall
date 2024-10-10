module Expr (Expr(..)) where

data Expr
  = NumExpr Double
  | SqrtExpr Expr
  | AddExpr Expr Expr
  | SubExpr Expr Expr
  | MulExpr Expr Expr
  | DivExpr Expr Expr
  | PowerExpr Expr Expr
  | VarExpr String
  | Let String Expr Expr
  deriving(Eq)

instance Show Expr where
  show (NumExpr a) = show a
  show (SqrtExpr a) = "sqrt(" ++ show a ++ ")"
  show (AddExpr a b) = "(" ++ show a ++ "+" ++ show b ++ ")"
  show (SubExpr a b) = "(" ++ show a ++ "-" ++ show b ++ ")"
  show (MulExpr a b) = "(" ++ show a ++ "*" ++ show b ++ ")"
  show (DivExpr a b) = "(" ++ show a ++ "/" ++ show b ++ ")"
  show (PowerExpr a b) = "(" ++ show a ++ "^" ++ show b ++ ")"
  show (VarExpr a) = a
  show (Let a b c) = "let " ++ a ++ " = " ++ show b ++ "in" ++ show c