module Main where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as M
import HW.Compiler
import HW.Eval
import HW.StackMachine
import Expr

complexExpr :: Expr String
complexExpr = Let "x" (Num 13) (Let "y" (Num 42) 
              (Let "x" (Plus (Var "x") (Var "y")) 
               (Plus (Var "x") (Var "y"))))

tests :: TestTree
tests = testGroup "Stack Machine Tests"
    [ testGroup "Simple Compilation Tests"
        [ testCase "Compile literal number" $
            compile (Num 42) @?= ([PushNum 42] :: StackProgram String)
        , testCase "Compile variable reference" $
            compile (Var "x") @?= ([PushVar "x"] :: StackProgram String)
        , testCase "Compile addition" $
            compile (Plus (Num 1) (Num 2)) @?= ([PushNum 1, PushNum 2, Add] :: StackProgram String)
        , testCase "Compile let binding" $
            compile (Let "x" (Num 13) (Var "x")) @?= ([PushNum 13, StoreVar "x", PushVar "x"] :: StackProgram String)
        ]
    , testGroup "Execution Tests"
        [ testCase "Execute addition" $ do
            let result = execProgram [PushNum 1, PushNum 2, Add] initialState
            result @?= Right (MachineState [3] M.empty)
        , testCase "Store and retrieve variable" $ do
            let result = execProgram [PushNum 42, StoreVar "x", PushVar "x"] initialState
            result @?= Right (MachineState [42] (M.fromList [("x", 42)]))
        ]
    , testGroup "Error Tests"
        [ testCase "Stack underflow on Add" $
            execProgram [PushNum 1, Add] initialState 
                @?= Left (StackUnderflow Add)
        , testCase "Undefined variable error" $
            execProgram [PushVar "x"] initialState 
                @?= Left (VarUndefined "\"x\"")
        , testCase "Stack not exhausted error" $
            execProgram [PushNum 1, PushNum 2] initialState 
                @?= Left (StackNotExhausted [2, 1])
        , testCase "Final stack empty error" $
            execProgram [] initialState 
                @?= Left FinalStackEmpty
        ]
    , testGroup "Complex Expression Tests"
        [ testCase "Complex expression compilation and execution" $ do
            let compiledProgram = compile complexExpr
            compiledProgram @?= 
                [ PushNum 13, StoreVar "x"
                , PushNum 42, StoreVar "y"
                , PushVar "x", PushVar "y", Add, StoreVar "x"
                , PushVar "x", PushVar "y", Add
                ]
            let result = execProgram compiledProgram initialState
            case result of
                Right (MachineState [v] _) -> v @?= 97
                _ -> assertFailure "Expected single value on stack"
        ]
    ]

main :: IO ()
main = defaultMain tests
