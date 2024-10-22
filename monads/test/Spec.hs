import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf (printf)
import Control.Monad (unless)
import qualified Data.Map as Map
import HW.StackMachine
import HW.Eval

-- Numeric operations: Push numbers and test basic arithmetic
numCases :: [(StackProgram String, Either (Error String) (MachineState String))]
numCases =
  [ ([PushNum 5], Right (MachineState [5] Map.empty))
  , ([PushNum 0], Right (MachineState [0] Map.empty))
  , ([PushNum (-10)], Right (MachineState [-10] Map.empty))
  ]

-- Arithmetic operations: Test addition and underflow error handling
arithmeticCases :: [(StackProgram String, Either (Error String) (MachineState String))]
arithmeticCases =
  [ ([PushNum 2, PushNum 4, Add], Right (MachineState [6] Map.empty))
  , ([PushNum 10, PushNum 5, Add], Right (MachineState [15] Map.empty))
  , ([PushNum 13, Add], Left (StackUnderflow Add))
  ]

-- Variable cases: Test variable storage and retrieval
varCases :: [(StackProgram String, Either (Error String) (MachineState String))]
varCases =
  [ ([PushNum 5, StoreVar "x", PushVar "x"], Right (MachineState [5] (Map.fromList [("x", 5)])))
  , ([PushNum 10, StoreVar "y", PushVar "y", PushNum 2, Add], Right (MachineState [12] (Map.fromList [("y", 10)])))
  , ([PushVar "x"], Left (VarUndefined "x"))
  , ([StoreVar "x"], Left (StackUnderflow (StoreVar "x")))
  ]

testProgram :: (StackProgram String, Either (Error String) (MachineState String)) -> TestTree
testProgram (program, expected) = testCase (printf "Testing program: %s" (show program)) $ do
    let actual = execProgram program initialState
    unless (expected == actual) $ assertFailure (printf "Expected: %s but got: %s" (show expected) (show actual))

numTests :: TestTree
numTests = testGroup "Numeric Tests" $ map testProgram numCases

arithmeticTests :: TestTree
arithmeticTests = testGroup "Arithmetic Tests" $ map testProgram arithmeticCases

varTests :: TestTree
varTests = testGroup "Variable Tests" $ map testProgram varCases

main :: IO ()
main = defaultMain $ testGroup "Stack Machine Tests" [numTests, arithmeticTests, varTests]