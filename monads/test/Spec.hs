module Main (main) where

import qualified Data.Map as M

import HW.StackMachine
import HW.Compiler
import HW.Eval
import Expr

import Test.Tasty.HUnit
import Test.Tasty


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ compilerTests
  , evalTests
  ]

compilerTests :: TestTree
compilerTests = testGroup "Compiler Tests"
  [ testCase "Compile single number" $
      assertEqual "" [PushNum 5] (compile (Num 5 :: Expr String))

  , testCase "Compile single variable" $
      assertEqual "" [PushVar "x"] (compile (Var "x" :: Expr String))

  , testCase "Compile addition" $
      assertEqual "" [PushNum 5, PushNum 10, Add] (compile (Plus (Num 5) (Num 10) :: Expr String))

  , testCase "Compile let expression" $
      assertEqual ""
        [PushNum 5, StoreVar "x", PushVar "x", PushNum 2, Add]
        (compile (Let "x" (Num 5) (Plus (Var "x") (Num 2)) :: Expr String))

  , testCase "Compile complex let expression" $
      assertEqual ""
        [PushNum 13, StoreVar "x", PushNum 42, StoreVar "y", PushVar "x", PushVar "y", Add, StoreVar "x", PushVar "x", PushVar "y", Add]
        (compile (Let "x" (Num 13) (Let "y" (Num 42) (Let "x" (Plus (Var "x") (Var "y")) (Plus (Var "x") (Var "y")))) :: Expr String))
  ]

evalTests :: TestTree
evalTests = testGroup "Evaluation Tests"
  [testCase "Evaluate a correct program" $
      assertEqual "" (Right (MachineState [17] (M.singleton "x" 15))) (execProgram exampleProgram initialState)

  , testCase "Stack underflow during addition" $
      assertEqual "" (Left (StackUnderflow Add)) (execProgram underflowExpr initialState)

  , testCase "Undefined variable" $
      assertEqual "" (Left (VarUndefined "\"x\"")) (execProgram undefVar initialState)

  , testCase "Stack not exhausted" $
      assertEqual "" (Left (StackNotExhausted [10, 5])) (execProgram [PushNum 5, PushNum 10] initialState)

  , testCase "Store variable" $
      assertEqual "" (Right (MachineState [7] (M.singleton "x" 5))) (execProgram [PushNum 5, StoreVar "x", PushVar "x", PushNum 2, Add] initialState)

  , testCase "Store more variables" $
      assertEqual ""
        (Right (MachineState [10] (M.fromList [("x", 5), ("y", 5)])))
        (execProgram
          [PushNum 5, StoreVar "x",
           PushNum 5, StoreVar "y",
           PushVar "x", PushVar "y", Add]
          initialState)
  ]
