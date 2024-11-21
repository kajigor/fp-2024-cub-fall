module Test.Compiler (props) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import HW.Compiler (compile)
import HW.Eval (execProgram, initialState, MachineState(..), Error(..))
import qualified Expr as E

import Test.Tasty
import Test.Tasty.Hedgehog

import qualified Data.Map as Map
import Control.Monad (when)

-- Base case generator for numbers
genNumExpr :: Gen (E.Expr String)
genNumExpr = E.Num <$> Gen.int (Range.linear (-1000) 1000)

-- Generator for variables (only if bound)
genVarExpr :: [String] -> Gen (E.Expr String)
genVarExpr [] = Gen.discard -- Cannot generate a variable if none are bound
genVarExpr boundVars = E.Var <$> Gen.element boundVars

-- Recursive generator for addition expressions
genAddExpr :: [String] -> Gen (E.Expr String)
genAddExpr boundVars = E.Plus <$> genExpr boundVars <*> genExpr boundVars

-- Recursive generator for let expressions
genLetExpr :: [String] -> Gen (E.Expr String)
genLetExpr boundVars = do
  -- Ensure we don't reuse or exceed a reasonable number of variables
  let availableVars = filter (`notElem` boundVars) ["x", "y", "z"]
  
  -- If no new variables are available, discard
  when (null availableVars) Gen.discard
  
  var <- Gen.element availableVars
  boundExpr <- genExpr boundVars
  let newBoundVars = var : boundVars
  bodyExpr <- genExpr newBoundVars
  return $ E.Let var boundExpr bodyExpr

-- Generalized expression generator
genExpr :: [String] -> Gen (E.Expr String)
genExpr boundVars = Gen.recursive Gen.choice
  (genNumExpr : (if null boundVars then [] else [genVarExpr boundVars])) 
  [ genAddExpr boundVars
  , genLetExpr boundVars
  ]

-- Change the environment type to a list of maps
type Env v = [Map.Map v Int]

-- Implemented an Interpreter
evalExpr :: (Ord v, Show v) => Env v -> E.Expr v -> Either String Int
evalExpr envs (E.Num n) = Right n
evalExpr envs (E.Var v) =
  case lookupVar v envs of
    Just val -> Right val
    Nothing -> Left $ "Undefined variable: " ++ show v
evalExpr envs (E.Plus l r) = do
  valL <- evalExpr envs l
  valR <- evalExpr envs r
  return $ valL + valR
evalExpr envs (E.Let v e b) = do
  val <- evalExpr envs e
  let newEnv = Map.singleton v val
  evalExpr (newEnv : envs) b

-- Helper function to look up variables in the environment stack
lookupVar :: Ord v => v -> Env v -> Maybe Int
lookupVar _ [] = Nothing
lookupVar v (env : rest) =
  case Map.lookup v env of
    Just val -> Just val
    Nothing -> lookupVar v rest

-- Generalized property
prop_compileAndExecuteExpr :: Property
prop_compileAndExecuteExpr = property $ do
  expr <- forAll $ genExpr []
  let program = compile expr
  let result = execProgram program initialState
  let expected = evalExpr [] expr  -- Start with an empty environment stack
  case (result, expected) of
    (Right finalState, Right expectedVal) -> do
      let stack = getStack finalState
      assert (head stack == expectedVal)
    (Left err, _) -> do
      footnote $ "Execution failed with error: " ++ show err
      failure
    (_, Left err) -> do
      footnote $ "Interpreter failed with error: " ++ err
      failure

props :: [TestTree]
props =
  [ testProperty "Compiling and executing expressions results in correct value" prop_compileAndExecuteExpr ]