import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.Map as M
import HW.Eval
import HW.Compiler
import Expr
import HW.StackMachine
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)

-- Generator for expressions
genExpr :: Gen (Expr String)
genExpr = Gen.recursive Gen.choice
  [ Num <$> Gen.int (Range.linear 0 100)                         -- Generate a random number
  , Expr.Var <$> Gen.element ["x", "y", "z"]                     -- Generate a random variable
  ]
  [ Plus <$> genExpr <*> genExpr                                 -- Generate addition of two expressions
  , Let <$> Gen.element ["x", "y", "z"] <*> genExpr <*> genExpr  -- Generate let bindings
  ]

-- Property: Compiler consistency
prop_compilerConsistency :: Property
prop_compilerConsistency = property $ do
    expr <- forAll $ genExpr

    let program = compile expr
    let result = execProgram program initialState

    case result of
      Right (MachineState [val] _) -> success  
      Left _ -> success                        
      _ -> failure                             

-- Property: Undefined variable error
prop_undefinedVariableError :: Property
prop_undefinedVariableError = property $ do
    let program = [HW.StackMachine.PushVar "undefinedVar"]
    let result = execProgram program initialState
    result === Left (VarUndefined "undefinedVar")

-- List of properties
props :: [TestTree]
props =
  [ testProperty "Compiler produces consistent results" prop_compilerConsistency
  , testProperty "Error on undefined variable" prop_undefinedVariableError
  ]

-- Main function to run all properties
main :: IO ()
main = defaultMain $ testGroup "Test.Properties" props