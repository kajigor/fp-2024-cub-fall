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
      let expr = LetExpr "x" (NumExpr 13) (
                 LetExpr "y" (NumExpr 42) (
                 LetExpr "x" (AddExpr (VarExpr "x") (VarExpr "y")) (
                 AddExpr (VarExpr "x") (VarExpr "y"))))
          compiled = compile expr
          result = execProgram compiled initialState
          expected = Right $ MachineState [97] []
      in result @?= expected

  , testCase "combined test for variable shadowing" $
      let expr = LetExpr "x" (NumExpr 5) (
                 LetExpr "x" (NumExpr 10) (
                 AddExpr (VarExpr "x") (NumExpr 20)))
          compiled = compile expr
          result = execProgram compiled initialState
          expected = Right $ MachineState [30] []
      in result @?= expected

  , testCase "combined test for basic addition" $
      let expr = AddExpr (NumExpr 3) (NumExpr 4)
          compiled = compile expr
          result = execProgram compiled initialState
          expected = Right $ MachineState [7] []
      in result @?= expected

  , testCase "combined test for storing and retrieving a variable" $
      let expr = LetExpr "x" (NumExpr 10) (AddExpr (VarExpr "x") (NumExpr 5))
          compiled = compile expr
          result = execProgram compiled initialState
          expected = Right $ MachineState [15] []
      in result @?= expected

  , testCase "combined test for undefined variable." $
      let expr = LetExpr "x" (NumExpr 10) (VarExpr "undefined")
          compiled = compile expr
          result = execProgram compiled initialState
          expected = Left (VarUndefined "undefined")
      in result @?= expected

  , testCase "combined test for stack underflow on Add" $
      let expr = LetExpr "x" (NumExpr 1) (
                 LetExpr "y" (NumExpr 2) (
                 AddExpr (VarExpr "x") (VarExpr "y")))
          compiled = compile expr
          result = execProgram (compiled ++ [Add]) initialState
          expected = Left (StackUnderflow Add)
      in result @?= expected
  ]