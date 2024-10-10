module Expr (Expr(..), Operator(..)) where


data Operator = OpAdd | OpSub | OpMul | OpDiv | OpPow
    deriving (Eq, Show)


data Expr
    = Lit Double
    | Var String
    | Unary String Expr
    | Binary Operator Expr Expr
    | Let String Expr Expr
    deriving (Eq)

instance Show Expr where
    show (Lit n) = show n
    show (Var v) = v
    show (Unary op expr) = op ++ "(" ++ show expr ++ ")"
    show (Binary op left right) = "(" ++ show left ++ " " ++ showOp op ++ " " ++ show right ++ ")"
    show (Let var expr body) = "let " ++ var ++ " = " ++ show expr ++ " in " ++ show body

showOp :: Operator -> String
showOp OpAdd = "+"
showOp OpSub = "-"
showOp OpMul = "*"
showOp OpDiv = "/"
showOp OpPow = "^"
