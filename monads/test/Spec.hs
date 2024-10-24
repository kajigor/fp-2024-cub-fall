module Main where

import Test.Tasty
import Test.Tasty.HUnit

import HW.Eval
import HW.Compiler
import HW.StackMachine

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Final Tests"
  [ compilerTests
  , evaluationTests
  ]

compilerTests :: TestTree
compilerTests = testGroup "Compilation Tests"
  [ testCase "compile nested let expressions with 13 and 42 from the comments" $
      let expr = LetExpr "x" (NumExpr 13) (
                 LetExpr "y" (NumExpr 42) (
                 LetExpr "x" (AddExpr (VarExpr "x") (VarExpr "y")) (
                 AddExpr (VarExpr "x") (VarExpr "y"))))
          expected = [PushNum 13, StoreVar "x",
                      PushNum 42, StoreVar "y",
                      PushVar "x", PushVar "y", Add, StoreVar "x",
                      PushVar "x", PushVar "y", Add]
          result = compile expr
      in result @?= expected

  , testCase "compile basic addition" $
      let expr = AddExpr (NumExpr 3) (NumExpr 4)
          expected = [PushNum 3, PushNum 4, Add]
          result = compile expr
      in result @?= expected

  , testCase "compile let with addition" $
      let expr = LetExpr "a" (NumExpr 5) (AddExpr (VarExpr "a") (NumExpr 10))
          expected = [PushNum 5, StoreVar "a", PushVar "a", PushNum 10, Add]
          result = compile expr
      in result @?= expected

  , testCase "compile let with nested variables" $
      let expr = LetExpr "x" (NumExpr 1) (
                 LetExpr "y" (NumExpr 2) (
                 AddExpr (VarExpr "x") (VarExpr "y")))
          expected = [PushNum 1, StoreVar "x",
                      PushNum 2, StoreVar "y",
                      PushVar "x", PushVar "y", Add]
          result = compile expr
      in result @?= expected
  ]

evalTests :: TestTree
evalTests = testGroup "Evaluation Tests"
  [ testCase "eval addition of two numbers" $
      let program = [PushNum 2, PushNum 3, Add]
          expected = Right $ MachineState [5] M.empty
          result = execProgram program initialState
      in result @?= expected

  , testCase "eval storing and retrieving a variable" $
      let program = [PushNum 10, StoreVar "x", PushVar "x"]
          expected = Right $ MachineState [10] (M.fromList [("x", 10)])
          result = execProgram program initialState
      in result @?= expected

  , testCase "eval nested let expressions with 13 and 42" $
      let program = [PushNum 13, StoreVar "x",
                     PushNum 42, StoreVar "y",
                     PushVar "x", PushVar "y", Add, StoreVar "x",
                     PushVar "x", PushVar "y", Add]
          expected = Right $ MachineState [97] (M.fromList [("x", 55), ("y", 42)])
          result = execProgram program initialState
      in result @?= expected

  , testCase "eval with undefined variable" $
      let program = [PushVar "undefined"]
          expected = Left (VarUndefined "undefined")
          result = execProgram program initialState
      in result @?= expected

  , testCase "eval with stack underflow on Add" $
      let program = [Add]
          expected = Left (StackUnderflow Add)
          result = execProgram program initialState
      in result @?= expected

  , testCase "eval stack not exhausted (extra elements)" $
      let program = [PushNum 1, PushNum 2]
          expected = Left (StackNotExhausted [1, 2])
          result = execProgram program initialState
      in result @?= expected

  , testCase "eval variable storage and addition" $
      let program = [PushNum 5, StoreVar "a", PushNum 10, PushVar "a", Add]
          expected = Right $ MachineState [15] (M.fromList [("a", 5)])
          result = execProgram program initialState
      in result @?= expected
  ]