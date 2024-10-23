module Expr where

data Expr
  = Number Double
  | SquareRoot Expr
  | Add Expr Expr
  | Subtract Expr Expr
  | Multiply Expr Expr
  | Divide Expr Expr
  | Power Expr Expr
  | Var String
  | Let String Expr Expr
  deriving Eq

instance Show Expr where
  show (Number n) = show n
  show (SquareRoot e) = "√(" ++ show e ++ ")"
  show (Add e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
  show (Subtract e1 e2) = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
  show (Multiply e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
  show (Divide e1 e2) = "(" ++ show e1 ++ " / " ++ show e2 ++ ")"
  show (Power e1 e2) = "(" ++ show e1 ++ " ^ " ++ show e2 ++ ")"
  show (Var name) = name
  show (Let name bindingExpr inExpr) = "let " ++ name ++ " = " ++ show bindingExpr ++ " in " ++ show inExpr
