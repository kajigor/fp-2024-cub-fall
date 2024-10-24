module Main where

import Test.Tasty
import Test.Tasty.HUnit

import HW.Eval
import HW.Compiler
import HW.StackMachine
import Control.Monad
import Expr

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Combined Compilation and Evaluation Tests"
  [ combinedTests
  ]

combinedTests :: TestTree
combinedTests = testGroup "Combined Tests"
  [ testCase "combined test for nested let expressions with 13 and 42" $
      let expr = Let "x" (Num 13) (
                 Let "y" (Num 42) (
                 Let "x" (Plus (Var "x") (Var "y")) (
                 Plus (Var "x") (Var "y"))))
          compiled = compile expr
          result = execProgram compiled initialState
          expected = Right $ MachineState [97] []
      in result @?= expected

  , testCase "combined test for variable shadowing" $
      let expr = Let "x" (Num 5) (
                 Let "x" (Num 10) (
                 Plus (Var "x") (Num 20)))
          compiled = compile expr
          result = execProgram compiled initialState
          expected = Right $ MachineState [30] []
      in result @?= expected

  , testCase "combined test for basic addition" $
      let expr = Plus (Num 3) (Num 4)
          compiled = compile expr
          result = execProgram compiled initialState
          expected = Right $ MachineState [7] []
      in result @?= expected

  , testCase "combined test for storing and retrieving a variable" $
      let expr = Let "x" (Num 10) (Plus (Var "x") (Num 5))
          compiled = compile expr
          result = execProgram compiled initialState
          expected = Right $ MachineState [15] []
      in result @?= expected

  , testCase "combined test for undefined variable" $
      let expr = Let "x" (Num 10) (Var "undefined")
          compiled = compile expr
          result = execProgram compiled initialState
          expected = Left (VarUndefined "undefined")
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