src/Expr.hs
module Expr
  ( Expr(..)
  , precedence
  , showExprPrec
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
  deriving (Eq)

precedence :: Expr -> Int
precedence expr = case expr of
  Num _ -> 5
  Var _ -> 5
  Let _ _ _ -> 0
  Sqrt _ -> 4
  Pow _ _ -> 3
  Mul _ _ -> 2
  Div _ _ -> 2
  Add _ _ -> 1
  Sub _ _ -> 1

parenthesize :: Int -> Int -> String -> String
parenthesize outerPrec innerPrec str
  | outerPrec > innerPrec = "(" ++ str ++ ")"
  | otherwise = str

showExprPrec :: Int -> Expr -> String
showExprPrec _ (Num x) = show x
showExprPrec _ (Var v) = v
showExprPrec p (Let v e1 e2) = parenthesize p 0 $ "let " ++ v ++ " = " ++ show e1 ++ " in " ++ show e2
showExprPrec _ (Sqrt e) = "sqrt(" ++ show e ++ ")"
showExprPrec p (Add e1 e2) = parenthesize p 1 $ showExprPrec 1 e1 ++ " + " ++ showExprPrec 1 e2
showExprPrec p (Sub e1 e2) = parenthesize p 1 $ showExprPrec 1 e1 ++ " - " ++ showExprPrec 2 e2
showExprPrec p (Mul e1 e2) = parenthesize p 2 $ showExprPrec 2 e1 ++ " * " ++ showExprPrec 2 e2
showExprPrec p (Div e1 e2) = parenthesize p 2 $ showExprPrec 2 e1 ++ " / " ++ showExprPrec 3 e2
showExprPrec p (Pow e1 e2) = parenthesize p 3 $ showExprPrec 3 e1 ++ " ^ " ++ showExprPrec 4 e2

instance Show Expr where
  show expr = showExprPrec 0 expr