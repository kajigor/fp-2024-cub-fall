{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.Map as M
import HW.Compiler (compile)
import HW.Eval (evalExpr, execProgram, initialState, MachineState(..), Error(..)) 
import HW.StackMachine (StackInstr(..)) 
import Expr (Expr(..)) 

-- hedgehog generators
genIntExpr :: Gen (Expr String)
genIntExpr = Num <$> Gen.int (Range.constant 0 100)

genVarExpr :: Gen (Expr String)
genVarExpr = Expr.Var <$> Gen.element ["x", "y", "z"]  

genComplexExpr :: Gen (Expr String)
genComplexExpr = do
    expr1 <- genIntExpr
    expr2 <- genIntExpr
    var <- Gen.element ["x", "y", "z"]
    Gen.choice
        [ return expr1
        , return expr2
        , Plus expr1 expr2 <$ Gen.constant ()
        , Let var expr1 expr2 <$ Gen.constant ()
        ]

-- property: compiling and executing give the same result as direct evaluation
prop_compile_eval :: Property
prop_compile_eval = property $ do
    expr <- forAll genComplexExpr
    let directEval = evalExpr expr M.empty
        compiledProgram = compile expr
        result = execProgram compiledProgram initialState
    case (directEval, result) of
        (Right expected, Right (MachineState [actual] _)) -> actual === expected
        _ -> failure

-- property: the result of double reversal should be the original list
prop_double_reverse :: Property
prop_double_reverse = property $ do
    exprList <- forAll $ Gen.list (Range.linear 0 10) genComplexExpr
    let reversedTwice = reverse (reverse exprList)
    reversedTwice === exprList

complexExpr :: Expr String
complexExpr = Let "x" (Num 13) (Let "y" (Num 42) 
              (Let "x" (Plus (Expr.Var "x") (Expr.Var "y")) 
               (Plus (Expr.Var "x") (Expr.Var "y"))))

tests :: TestTree
tests = testGroup "Stack Machine Tests"
    [ testGroup "Simple Compilation Tests"
        [ testCase "Compile literal number" $
            compile (Num 42) @?= ([PushNum 42] :: [StackInstr String])
        , testCase "Compile variable reference" $
            compile (Expr.Var "x") @?= ([PushVar "x"] :: [StackInstr String])
        , testCase "Compile addition" $
            compile (Plus (Num 1) (Num 2)) @?= ([PushNum 1, PushNum 2, Add] :: [StackInstr String])
        , testCase "Compile let binding" $
            compile (Let "x" (Num 13) (Expr.Var "x")) @?= ([PushNum 13, StoreVar "x", PushVar "x"] :: [StackInstr String]) 
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
    , testGroup "Hedgehog Property Tests"
        [ testProperty "Compile and execute matches direct evaluation" prop_compile_eval
        , testProperty "Double reversal property" prop_double_reverse
        ]
    ]

main :: IO ()
main = defaultMain tests
