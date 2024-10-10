module Expr (Expr(..), Operator(..)) where

import Text.Printf (printf)

data Operator = Add 
     | Sub 
     | Mult 
     | Div 
     | Pow
     deriving (Eq)
     
instance Show Operator where
     show Add = "+"
     show Sub = "-"
     show Mult = "*"
     show Div = "/"
     show Pow = "^"

data Expr = Num Double 
     | Sqrt Expr 
     | CompExpr Operator Expr Expr
     | Var String
     | Let String Expr Expr
     deriving (Eq)

instance Show Expr where
    show (Num a) = show a
    show (Sqrt a) = printf "sqrt(%s)" (show a)
    show (CompExpr op a b) = printf "(%s %s %s)" (show a) (show op) (show b)
    show (Var str) = show str
    show (Let str a b) = printf "let %s = %s in %s" (show str) (show a) (show b)
