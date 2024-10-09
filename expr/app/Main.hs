module Main (main) where

import Expr 
import Interpreter
import qualified Data.Map.Strict as M


main :: IO ()
main = do
  let expr1 = Let (Var "x") (Num 4) (Add (Var "x") (Num 3))  -- let x = 4 in x + 3
  let expr2 = Let (Var "x") (Num 4) (Let (Var "y") (Num 2) (Mul (Var "x") (Var "y")))  -- let x = 4 in let y = 2 in x * y
  let expr3 = Let (Var "x") (Num 4) (Sqrt (Var "x"))  -- let x = 4 in sqrt(x)
  let expr4 = Let (Var "x") (Num 2) (Let (Var "y") (Mul (Var "x") (Num 2)) (Let (Var "z") (Add (Var "y") (Num 3)) (Pow (Var "z") (Var "x"))))
  print $ eval M.empty expr1   -- Right 7.0
  print $ eval M.empty expr2   -- Right 8.0
  print $ eval M.empty expr3   -- Right 2.0
  print $ eval M.empty expr4   -- Right 49.0 
