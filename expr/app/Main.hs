module Main (main) where

import Expr 
import Interpreter
import qualified Data.Map.Strict as M


main :: IO ()
main = do
  let expr1 = Let "x" (Number 4) (Add (Var "x") (Number 3))  -- let x = 4 in x + 3
  let expr2 = Let "x" (Number 4) (Let "y" (Number 2) (Mult (Var "x") (Var "y")))  -- let x = 4 in let y = 2 in x * y
  let expr3 = Let "x" (Number 4) (Sqrt (Var "x"))  -- let x = 4 in sqrt(x)
  let expr4 = Let "x" (Number 2) (Let "y" (Mult (Var "x") (Number 2)) (Let "z" (Add (Var "y") (Number 3)) (Exp (Var "z") (Var "x"))))
  print $ eval M.empty expr1   -- Right 7.0
  print $ eval M.empty expr2   -- Right 8.0
  print $ eval M.empty expr3   -- Right 2.0
  print $ eval M.empty expr4   -- Right 49.0 
