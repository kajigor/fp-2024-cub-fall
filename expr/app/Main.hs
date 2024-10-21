module Main (main) where

import qualified Data.Map as Map
import Expr (Expr(..))
import Error (Error(..))
import Interpreter (eval)


main :: IO ()
main = do
  putStrLn "Running expression evaluator tests..."

