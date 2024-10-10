module Main (main) where

import Control.Monad (unless)
import Text.Printf (printf)
import Data.Either (either)

import Lib
import BaseExpr
import Expr
import qualified Data.Map.Strict as M

main :: IO ()
main = putStrLn $ show $ eval M.empty (Let "y" (Var "x") $ Let "x" (Number 1) $ Plus (Var "x") (Number 1))