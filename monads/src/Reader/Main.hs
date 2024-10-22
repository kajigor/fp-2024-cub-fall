module Reader.Main where

import Expr
import Reader.Eval (runEval, runEvalSafe)
import Text.Printf
import Util

main :: IO ()
main = do
  putStrLn (thickEnclose "Eval examples")
  mapM_ (run runEval) exprs

  putStrLn (thickEnclose "Eval Safe examples")
  mapM_ (run runEvalSafe) (Plus (Var "x") (Num 13) : exprs)
  where
    run eval e = do
      print e
      putStrLn $ printf "Result: %s\n" (show $ eval e)