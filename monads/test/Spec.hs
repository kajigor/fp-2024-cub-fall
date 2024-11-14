import Test.Tasty
import Test.Tasty.HUnit
import UnitTest
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Expr 
import HW.Compiler
import HW.StackMachine
import HW.Eval (execProgram, initialState, Error (VarUndefined), MachineState(..), getStack)
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)
import Data.Either (isLeft)
import Reader.Eval (runEvalSafe)
import State.Renamer (runRename)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set

genInt :: Gen Int
genInt = Gen.int (Range.constant (-100) 100)


genString :: Int -> Int -> Gen String
genString minLength maxLength = Gen.list (Range.constant minLength maxLength) Gen.alphaNum

genVar :: Gen [String]
genVar = Gen.list (Range.constant 1 10) $ genString 1 10

{-
genExpr :: [String] -> Gen (Expr.Expr String)
genExpr variableNames = Gen.recursive Gen.choice [
      Expr.Num <$> genInt,
      Expr.Var <$> Gen.element variableNames
  ] [
    Expr.Plus <$> genExpr variableNames <*> genExpr variableNames,
    Expr.Let <$> Gen.element variableNames <*> genExpr variableNames <*> genExpr variableNames
  ]
-}

genExpr :: [String] -> Gen (Expr.Expr String)
genExpr variableNames = genExprWithScope (Set.fromList variableNames)

genExprWithScope :: Set.Set String -> Gen (Expr.Expr String)
genExprWithScope allowedVars = Gen.recursive Gen.choice [
      Expr.Num <$> genInt,
      Expr.Var <$> Gen.element (Set.toList allowedVars)
  ] [
    Expr.Plus <$> genExprWithScope allowedVars <*> genExprWithScope allowedVars,
    genLetExpr allowedVars
  ]

genLetExpr :: Set.Set String -> Gen (Expr.Expr String)
genLetExpr allowedVars = do
  newVar <- genString 1 10 
  expr <- genExprWithScope allowedVars  
  
  let updatedScope = Set.delete newVar allowedVars
  body <- genExprWithScope updatedScope
  return $ Expr.Let newVar expr body


propConsistentEvaluation :: Property
propConsistentEvaluation = property $ do
  variableNames <- forAll genVar        
  expr <- forAll (genExpr variableNames)
  let evalResult = runEvalSafe expr

  let program = compile expr
  let programResult = case execProgram program initialState of
        Left _ -> Nothing 
        Right finalState -> case getStack finalState of
          [result] -> Just result 
          _        -> Nothing
  evalResult === programResult

prop_undefinedVariableError :: Property
prop_undefinedVariableError = property $ do
    variableNames <- forAll genVar
    expr <- forAll $ genExpr variableNames
    let stackProgram = compile $ Expr.Let "candela" expr (Expr.Var "candelo")
    Hedgehog.assert (isLeft $ execProgram stackProgram initialState)  

main :: IO ()
main = defaultMain $ testGroup "Stack Machine Tests"
  [ testCompile
  , testExecInstr
  , testExecProgram
  , testProperty "Hedgehog: Consistent evaluation" propConsistentEvaluation
  , testProperty "Error is raised if an undefined variable is used" prop_undefinedVariableError
  ]