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

instance Eq Expr where
  NumExpr a == NumExpr b = a == b
  SqrtExpr a == SqrtExpr b = a == b
  AddExpr a b == AddExpr c d = a == c && b == d
  SubExpr a b == SubExpr c d = a == c && b == d
  MulExpr a b == MulExpr c d = a == c && b == d
  DivExpr a b == DivExpr c d = a == c && b == d
  PowerExpr a b == PowerExpr c d = a == c && b == d
  VarExpr a == VarExpr b = a == b
  Let a b c == Let d e f = a == d && b == e && c == f
  _ == _ = False