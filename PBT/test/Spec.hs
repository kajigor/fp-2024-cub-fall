{-# LANGUAGE OverloadedStrings #-}

module Test.Compiler where

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Data.Map (Map)
import qualified Data.Map as M

import           Control.Monad (when)
import           System.Exit (exitFailure)

import           Expr (Expr(..))
import           HW.Compiler (compile)
import           HW.StackMachine (StackInstr(..), StackProgram, execProgram, initialState, getStack, Error(..))
import           Interpreter (evalExpr)

genNum :: Gen Int
genNum = Gen.int (Range.constant (-1000) 1000)

genVar :: Gen String
genVar = Gen.element ["x", "y", "z"]

genNumExpr :: Gen (Expr String)
genNumExpr = Num <$> genNum

genVarExpr :: Gen (Expr String)
genVarExpr = Var <$> genVar

genPlusExpr :: Gen (Expr String)
genPlusExpr = Plus <$> genExpr <*> genExpr

genLetExpr :: Gen (Expr String)
genLetExpr = Let <$> genVar <*> genExpr <*> genExpr

genExpr :: Gen (Expr String)
genExpr = Gen.recursive Gen.choice [genNumExpr, genVarExpr] [genPlusExpr, genLetExpr]

-- Property tests

prop_directEvaluationConsistency :: Property
prop_directEvaluationConsistency = property $ do
  expr <- forAll genExpr
  let evalResult = evalExpr M.empty expr
  let compiledProgram = compile expr
  let execResult = case execProgram compiledProgram initialState of
        Left err -> Left err
        Right finalState ->
          case getStack finalState of
            [result] -> Right result
            stack    -> Left (StackNotExhausted stack)
  evalResult === execResult

prop_addInstructionCount :: Property
prop_addInstructionCount = property $ do
  expr <- forAll genExpr
  let compiled = compile expr
  let addInstrCount = length $ filter isAddInstr compiled
  addInstrCount === countAdditions expr

isAddInstr :: StackInstr v -> Bool
isAddInstr Add = True
isAddInstr _   = False

countAdditions :: Expr v -> Int
countAdditions (Num _)      = 0
countAdditions (Var _)      = 0
countAdditions (Plus a b)   = 1 + countAdditions a + countAdditions b
countAdditions (Let _ e1 e2) = countAdditions e1 + countAdditions e2


prop_addZeroIdentity :: Property
prop_addZeroIdentity = property $ do
  num <- forAll genNum
  let expr = Plus (Num num) (Num 0)
  let evalResult = evalExpr M.empty expr
  evalResult === Right num

prop_variableStorage :: Property
prop_variableStorage = property $ do
  varName <- forAll genVar
  num <- forAll genNum
  let expr = Let varName (Num num) (Var varName)
  let evalResult = evalExpr M.empty expr
  evalResult === Right num

prop_plusWithZero :: Property
prop_plusWithZero = property $ do
  expr <- forAll genExpr
  let exprWithZero = Plus expr (Num 0)
  let evalResultOriginal = evalExpr M.empty expr
  let evalResultWithZero = evalExpr M.empty exprWithZero
  evalResultOriginal === evalResultWithZero

tests :: IO Bool
tests = checkParallel $ Group "Test.Compiler"
  [ ("prop_directEvaluationConsistency", prop_directEvaluationConsistency)
  , ("prop_addInstructionCount", prop_addInstructionCount)
  , ("prop_addZeroIdentity", prop_addZeroIdentity)
  , ("prop_variableStorage", prop_variableStorage)
  , ("prop_plusWithZero", prop_plusWithZero)
  ]

main :: IO ()
main = do
  result <- tests
  when (not result) exitFailure
