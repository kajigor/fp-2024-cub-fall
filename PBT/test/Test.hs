module Test (main) where

import Test.Tasty
import qualified Test.Compiler
import qualified Test.Interpreter

main :: IO ()
main = defaultMain $ testGroup "Property-based Tests"
  [ testGroup "Compiler Tests" Test.Compiler.props
  , testGroup "Interpreter Tests" Test.Interpreter.props
  ]