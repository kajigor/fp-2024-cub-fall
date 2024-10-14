module Expr where

import qualified Data.Map.Strict as M

data Expr
  = Number Double
  | Var String
  | Let String Expr Expr
  | Sqrt Expr
  | Plus Expr Expr
  | Minus Expr Expr
  | Prod Expr Expr
  | Divide Expr Expr
  | Exp Expr Expr
  deriving (Eq)

instance Show Expr where
  show (Number x) = show x
  show (Var x) = x
  show (Let x expr body) = "let " ++ x ++ " = " ++ show expr ++ " in " ++ show body
  show (Sqrt x) = "sqrt(" ++ show x ++ ")"
  show (Plus a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
  show (Minus a b) = "(" ++ show a ++ " - " ++ show b ++ ")"
  show (Prod a b) = "(" ++ show a ++ " * " ++ show b ++ ")"
  show (Divide a b) = "(" ++ show a ++ " / " ++ show b ++ ")"
  show (Exp a b) = "(" ++ show a ++ " ^ " ++ show b ++ ")"

instance Num Expr where
  (+) = Plus
  (*) = Prod
  abs = undefined
  signum = undefined
  fromInteger = Number . fromInteger
  negate x = Prod (Number (-1)) x
