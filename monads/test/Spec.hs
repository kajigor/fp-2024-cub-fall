module Main where

import Test.Tasty
import Test.Tasty.HUnit

import HW.Eval
import HW.Compiler
import HW.StackMachine
import Control.Monad
import Expr

import qualified Data.Map as M

main :: IO ()
main = defaultMain combinedTests

combinedTests :: TestTree
combinedTests = testGroup "Combined tests for eval and compiler"
  [ testCase "combined test for nested let expressions with 13 and 42" $
      let expr = Let "x" (Num 13) (
                 Let "y" (Num 42) (
                 Let "x" (Plus (Var "x") (Var "y")) (
                 Plus (Var "x") (Var "y"))))
          compiled = compile expr
          result = execProgram compiled initialState
          expected = Right $ MachineState [97] (M.fromList [("x", 55), ("y", 42)])
      in result @?= expected

  , testCase "combined test for variable shadowing" $
      let expr = Let "x" (Num 5) (
                 Let "x" (Num 10) (
                 Plus (Var "x") (Num 20)))
          compiled = compile expr
          result = execProgram compiled initialState
          expected = Right $ MachineState [30] (M.fromList [("x", 10)])
      in result @?= expected

  , testCase "combined test for basic addition" $
      let expr = Plus (Num 3) (Num 4)
          compiled = compile expr
          result = execProgram compiled initialState
          expected = Right $ MachineState [7] M.empty
      in result @?= expected

  , testCase "combined test for storing and retrieving a variable" $
      let expr = Let "x" (Num 10) (Plus (Var "x") (Num 5))
          compiled = compile expr
          result = execProgram compiled initialState
          expected = Right $ MachineState [15] (M.fromList [("x", 10)])
      in result @?= expected

  , testCase "combined test for undefined variable" $
      let expr = Let "x" (Num 10) (Var "y")
          compiled = compile expr
          result = execProgram compiled initialState
          expected = Left (VarUndefined "\"y\"")
      in result @?= expected

  , testCase "combined test for stack underflow on Add" $
      let expr = Let "x" (Num 1) (
                 Let "y" (Num 2) (
                 Plus (Var "x") (Var "y")))
          compiled = compile expr
          result = execProgram (compiled ++ [Add]) initialState
          expected = Left (StackUnderflow Add)
      in result @?= expected
  ]