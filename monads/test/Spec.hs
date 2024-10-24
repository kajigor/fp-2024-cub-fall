module Main where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map as M

import qualified Expr as E
import HW.Compiler
import HW.Eval
import HW.StackMachine

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Test Cases"
  [ test_simple_number
  , test_variable
  , test_addition
  , test_nested_let
  , test_stack_underflow         
  , test_undefined_variable      
  , test_stack_not_exhausted  
  , test_complex_expression
  , test_store_var_empty_stack
  , test_addition_one_element
  ]

test_simple_number :: TestTree
test_simple_number = testCase "Simple Number" $ do
  let expr :: E.Expr String
      expr = E.Num 42

      expectedProgram :: StackProgram String
      expectedProgram = [PushNum 42]

      expectedState = MachineState [42] (M.empty :: M.Map String Int)
  let compiledProgram = compile expr
  assertEqual "Compile Num 42" expectedProgram compiledProgram
  let result = execProgram compiledProgram initialState
  case result of
    Right finalState -> assertEqual "Execute Num 42" expectedState finalState
    Left err -> assertFailure $ "Unexpected error: " ++ show err

test_variable :: TestTree
test_variable = testCase "Variable" $ do
  let expr :: E.Expr String
      expr = E.Let "x" (E.Num 10) (E.Var "x")

      expectedProgram :: StackProgram String
      expectedProgram = [PushNum 10, StoreVar "x", PushVar "x"]

      expectedState = MachineState [10] (M.fromList [("x", 10)])
  let compiledProgram = compile expr
  assertEqual "Compile Let x = 10 in x" expectedProgram compiledProgram
  let result = execProgram compiledProgram initialState
  case result of
    Right finalState -> assertEqual "Execute Let x = 10 in x" expectedState finalState
    Left err -> assertFailure $ "Unexpected error: " ++ show err

test_addition :: TestTree
test_addition = testCase "Addition" $ do
  let expr :: E.Expr String
      expr = E.Plus (E.Num 5) (E.Num 7)

      expectedProgram :: StackProgram String
      expectedProgram = [PushNum 5, PushNum 7, Add]

      expectedState = MachineState [12] (M.empty :: M.Map String Int)
  let compiledProgram = compile expr
  assertEqual "Compile 5 + 7" expectedProgram compiledProgram
  let result = execProgram compiledProgram initialState
  case result of
    Right finalState -> assertEqual "Execute 5 + 7" expectedState finalState
    Left err -> assertFailure $ "Unexpected error: " ++ show err

test_nested_let :: TestTree
test_nested_let = testCase "Nested Let" $ do
  let expr :: E.Expr String
      expr = E.Let "x" (E.Num 13)
               (E.Let "y" (E.Num 42)
                 (E.Let "x" (E.Plus (E.Var "x") (E.Var "y"))
                   (E.Plus (E.Var "x") (E.Var "y"))))

      expectedProgram :: StackProgram String
      expectedProgram = [ PushNum 13, StoreVar "x",
                          PushNum 42, StoreVar "y",
                          PushVar "x", PushVar "y", Add, StoreVar "x",
                          PushVar "x", PushVar "y", Add ]

      expectedState = MachineState [97] (M.fromList [("x", 55), ("y", 42)])
  let compiledProgram = compile expr
  assertEqual "Compile nested let expression" expectedProgram compiledProgram

test_stack_underflow :: TestTree
test_stack_underflow = testCase "Stack Underflow" $ do
  let program = [Add]
  let result = execProgram program initialState
  case result of
    Left (StackUnderflow Add) -> return ()
    _ -> assertFailure "Expected StackUnderflow Add error"

test_undefined_variable :: TestTree
test_undefined_variable = testCase "Undefined Variable" $ do
  let program = [PushVar "x"]
  let result = execProgram program initialState
  case result of
    Left (VarUndefined var) -> assertEqual "Undefined variable name" "\"x\"" var
    _ -> assertFailure "Expected VarUndefined error"

test_stack_not_exhausted :: TestTree
test_stack_not_exhausted = testCase "Stack Not Exhausted" $ do
  let program = [PushNum 1, PushNum 2]
  let result = execProgram program initialState
  case result of
    Left (StackNotExhausted stack) -> assertEqual "Stack not exhausted" [2, 1] stack
    _ -> assertFailure "Expected StackNotExhausted error"

test_complex_expression :: TestTree
test_complex_expression = testCase "Complex Expression" $ do
  let expr = E.Let "a" (E.Num 5)
               (E.Let "b" (E.Num 3)
                 (E.Plus (E.Plus (E.Var "a") (E.Var "b")) (E.Num 2)))
      expectedProgram = [ PushNum 5, StoreVar "a",
                          PushNum 3, StoreVar "b",
                          PushVar "a", PushVar "b", Add, PushNum 2, Add ]
      expectedState = MachineState [10] (M.fromList [("a", 5), ("b", 3)])
  let compiledProgram = compile expr
  assertEqual "Compile complex expression" expectedProgram compiledProgram
  let result = execProgram compiledProgram initialState
  case result of
    Right finalState -> assertEqual "Execute complex expression" expectedState finalState
    Left err -> assertFailure $ "Unexpected error: " ++ show err

test_store_var_empty_stack :: TestTree
test_store_var_empty_stack = testCase "StoreVar Empty Stack" $ do
  let program = [StoreVar "x"]
  let result = execProgram program initialState
  case result of
    Left (StackUnderflow instr) -> assertEqual "Stack underflow on StoreVar" (StoreVar "x") instr
    _ -> assertFailure "Expected StackUnderflow error on StoreVar"

test_addition_one_element :: TestTree
test_addition_one_element = testCase "Addition One Element" $ do
  let program = [PushNum 1, Add]
  let result = execProgram program initialState
  case result of
    Left (StackUnderflow Add) -> return ()
    _ -> assertFailure "Expected StackUnderflow Add error"
