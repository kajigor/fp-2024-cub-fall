module Expr where

import Control.Monad (unless)
import qualified Data.Map.Strict as M
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
    show (Let str a b) = prinf "let %s = %s in %s" (show str) (show a) (show b)

run :: Expr -> M.Map String Expr -> IO ()
run expr state = do
  print expr
  print state
  print (eval state expr)
  putStrLn ""

main = do
  let expr1 = Var "x"
  let expr2 = CompExpr Add (Num 2) (Num 2)
  let expr3 = CompExpr Add (Var "x") (Num 1)
  let state1 = M.fromList [("x", Num 42), ("y", Num 13)]
  let state2 = M.empty
  run expr1 state1
  run expr2 state1
  run expr3 state1
  run expr1 state2
  run expr2 state2
  run expr3 state2
