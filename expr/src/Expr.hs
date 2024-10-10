module Expr (Expr(..)) where

import GHC.Num (integerToInt)
import Text.Printf (printf)

data Expr
  = Lit Double
  | Sqrt Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  | Abs Expr
  | Sig Expr
  | Neg Expr
  | Let String Expr Expr
  | Var String
  deriving (Eq)

instance Show Expr where
  show (Lit n) = show n
  show (Add x y) = printf "(%s + %s)" (show x) (show y)
  show (Sub x y) = printf "(%s - %s)" (show x) (show y)
  show (Mul x y) = printf "(%s * %s)" (show x) (show y)
  show (Div x y) = printf "(%s / %s)" (show x) (show y)
  show (Pow x y) = printf "(%s ^ %s)" (show x) (show y)
  show (Sqrt x) = printf "sqrt(%s)" (show x)
  show (Abs x) = printf "abs(%s)" (show x)
  show (Sig x) = printf "sign(%s)" (show x)
  show (Let k v x) = printf "let %s = %s in %s" k (show v) (show x)
  show (Neg x) = printf "-(%s)" (show x)
  show (Var v) = v

instance Num Expr where
  (+) = Add
  (*) = Mul
  abs = Abs
  signum = Sig
  fromInteger = Lit . fromIntegral
  negate = Neg
