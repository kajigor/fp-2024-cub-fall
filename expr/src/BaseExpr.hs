module BaseExpr where

data Expr = Number Double
           | Var String
           | Let String Expr Expr  -- first Expr is what is assinged to the new "variable", and second Expr is the one being evaluated
           | Sqrt Expr 
           | Plus Expr Expr 
           | Minus Expr Expr 
           | Mult Expr Expr 
           | Div Expr Expr 
           | Pow Expr Expr
           | Abs Expr
           | Sgn Expr
           | UnaryMinus Expr
  deriving Eq

instance Show Expr where
  show (Number x) = show x
  show (Var name) = name
  show (Let name val e) = "let " ++ name ++ " = " ++ show val ++ " in " ++ show e
  show (Sqrt e) = "sqrt(" ++ show e ++ ")"
  show (Plus e1 e2) = "(" ++ show e1 ++ ") + (" ++ show e2 ++ ")"
  show (Minus e1 e2) = "(" ++ show e1 ++ ") - (" ++ show e2 ++ ")"
  show (Mult e1 e2) = "(" ++ show e1 ++ ") * (" ++ show e2 ++ ")"
  show (Div e1 e2) = "(" ++ show e1 ++ ") / (" ++ show e2 ++ ")"
  show (Pow e1 e2) = "(" ++ show e1 ++ ") ^ (" ++ show e2 ++ ")"
  show (Abs e) = "abs(" ++ show e ++ ")"
  show (Sgn e) = "signum(" ++ show e ++ ")"
  show (UnaryMinus e) = "-(" ++ show e ++ ")"
