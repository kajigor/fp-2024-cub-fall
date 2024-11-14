import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import HW.Compiler
import HW.StackMachine
import HW.Eval
import Expr (Expr(..))

genExpr :: Hedgehog.Gen (Expr String)
genExpr = Gen.recursive Gen.choice 
  [ Num <$> Gen.int (Range.linear 0 100)
  , Var <$> Gen.element ["x", "y", "z"]
  ]
  [ Let <$> Gen.element ["x", "y", "z"] <*> genExpr <*> genExpr
  , Plus <$> genExpr <*> genExpr
  ]

prop_noExtraStackElements :: Hedgehog.Property
prop_noExtraStackElements = Hedgehog.property $ do
  expr <- Hedgehog.forAll genExpr
  let compiled = compile expr
  case execProgram compiled initialState of
    Right finalState -> do
      let result = getStack finalState
      Hedgehog.assert $ length result == 1
    Left err -> Hedgehog.annotate $ "Execution failed with error: " ++ show err

prop_stackUnderflow :: Hedgehog.Property
prop_stackUnderflow = Hedgehog.property $ do
  expr <- Hedgehog.forAll genExpr
  let compiled = compile expr
  case execProgram compiled initialState of
    Left (StackUnderflow _) -> Hedgehog.success
    Left err -> Hedgehog.annotate $ "Unexpected error: " ++ show err
    Right _ -> Hedgehog.annotate "Expected a StackUnderflow error, but execution succeeded"

prop_varUndefined :: Hedgehog.Property
prop_varUndefined = Hedgehog.property $ do
  expr <- Hedgehog.forAll genExpr
  let compiled = compile expr
  case execProgram compiled initialState of
    Left (VarUndefined _) -> Hedgehog.success
    Left err -> Hedgehog.annotate $ "Unexpected error: " ++ show err
    Right _ -> Hedgehog.annotate "Expected a VarUndefined error, but execution succeeded"

prop_stackNotExhausted :: Hedgehog.Property
prop_stackNotExhausted = Hedgehog.property $ do
  expr <- Hedgehog.forAll genExpr
  let compiled = compile expr
  case execProgram compiled initialState of
    Left (StackNotExhausted _) ->  Hedgehog.success
    Left err -> Hedgehog.annotate $ "Unexpected error: " ++ show err
    Right _ -> Hedgehog.annotate "Expected a StackNotExhausted error, but execution succeeded"

tests :: TestTree
tests = testGroup "Property-Based Tests"
  [ 
    testProperty "No extra stack elements" prop_noExtraStackElements
  , testProperty "Stack Underflow" prop_stackUnderflow
  , testProperty "Undefined Variable" prop_varUndefined
  , testProperty "Stack not Exhausted" prop_stackNotExhausted
  ]

main :: IO ()
main = defaultMain tests
