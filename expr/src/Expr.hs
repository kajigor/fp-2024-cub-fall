module Expr where

import Text.Printf (printf)

data Expr
  = Numb Double
  | Var String
  | SqrtExpr Expr
  | BinOpAdd Expr Expr
  | BinOpSub Expr Expr
  | BinOpMul Expr Expr
  | BinOpDiv Expr Expr
  | BinOpPow Expr Expr
  | LetExpr String Expr Expr
  deriving (Eq)

instance Show Expr where
  show (Numb a) = printf "%s" (show a)
  show (Var s) = s
  show (SqrtExpr a) = printf "sqrt %s" (show a)
  show (BinOpAdd a b) = printf "(%s + %s)" (show a) (show b)
  show (BinOpSub a b) = printf "(%s - %s)" (show a) (show b)
  show (BinOpMul a b) = printf "(%s * %s)" (show a) (show b)
  show (BinOpDiv a b) = printf "(%s / %s)" (show a) (show b)
  show (BinOpPow a b) = printf "(%s ^ %s)" (show a) (show b)
  show (LetExpr s v stmt) = printf "(let %s = %s in %s)" s (show v) (show stmt)