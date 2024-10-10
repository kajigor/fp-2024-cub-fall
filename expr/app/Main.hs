module Main (main) where

import qualified Data.Map.Strict as M

import Lib

run :: Lib.Expr -> M.Map String Double -> IO ()
run expr state = do
  print expr
  print state
  print (eval state expr)
  putStrLn ""

main = do
  let expr1 = Var "x"
  let expr2 = Add (Lit 2) (Lit 2)
  let expr3 = Add (Var "x") (Lit 1)
  let state1 = M.fromList [("x", 42), ("y", 13)]
  let state2 = M.empty
  run expr1 state1
  run expr2 state1
  run expr3 state1
  run expr1 state2
  run expr2 state2
  run expr3 state2
