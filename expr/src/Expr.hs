module Expr where
import Text.Printf(printf)

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


showsPrec :: Int -> Expr -> String
showsPrec p expr = case expr of
  Num x -> show x
  Var v -> v
  Let v e1 e2 -> parenthesize p 0 $ "let " ++ v ++ " = " ++ show e1 ++ " in " ++ show e2
  Sqrt e -> "sqrt(" ++ showsPrec 4 e ++ ")"
  Add e1 e2 -> parenthesize p 1 $ showsPrec 1 e1 ++ " + " ++ showsPrec 1 e2
  Sub e1 e2 -> parenthesize p 1 $ showsPrec 1 e1 ++ " - " ++ showsPrec 2 e2
  Mul e1 e2 -> parenthesize p 2 $ showsPrec 2 e1 ++ " * " ++ showsPrec 2 e2
  Div e1 e2 -> parenthesize p 2 $ showsPrec 2 e1 ++ " / " ++ showsPrec 3 e2
  Pow e1 e2 -> parenthesize p 3 $ showsPrec 3 e1 ++ " ^ " ++ showsPrec 4 e2
  where
    parenthesize outerPrec innerPrec str
      | outerPrec > innerPrec = "(" ++ str ++ ")"
      | otherwise = str

instance Show Expr where
  show expr = showsPrec 0 expr