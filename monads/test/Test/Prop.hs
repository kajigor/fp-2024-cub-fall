module Test.Prop where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Expr
import HW.Compiler
import HW.StackMachine
import HW.Eval (execProgram, initialState, Error (VarUndefined))
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)
import Data.Either (isLeft)

genInt :: Gen Int
genInt = Gen.int (Range.constant (-100) 100)

genString :: Int -> Int -> Gen String
genString minLength maxLength = Gen.list (Range.constant minLength maxLength) Gen.alphaNum

genVarNames :: Gen [String]
genVarNames = Gen.list (Range.constant 1 10) $ genString 1 10

genExpr :: [String] -> Gen (Expr.Expr String)
genExpr varList = Gen.recursive Gen.choice [
        Expr.Num <$> genInt
        , Expr.Var <$> Gen.element varList
    ] [
        Expr.Plus <$> genExpr varList <*> genExpr varList
        , Expr.Let <$> Gen.element varList <*> genExpr varList <*> genExpr varList
    ]

plusCount :: Expr.Expr v -> Int
plusCount (Expr.Plus e1 e2) = plusCount e1 + plusCount e2 + 1
plusCount (Expr.Let _ e1 e2) = plusCount e1 + plusCount e2
plusCount _ = 0

prop_noUnneededAdditions :: Property
prop_noUnneededAdditions = property $ do
    varList <- forAll genVarNames
    expr <- forAll $ genExpr varList
    let stackProgram = compile expr
    assert (plusCount expr == length (filter (== Add) stackProgram))

prop_undefinedVariableError :: Property
prop_undefinedVariableError = property $ do
    varList <- forAll genVarNames
    expr <- forAll $ genExpr varList
    let stackProgram = compile $ Expr.Let "aaaaaaaaaaa" expr (Expr.Var "ooooooooooo")
    assert (isLeft $ execProgram stackProgram initialState)

props :: [TestTree]
props = [ 
        testProperty "The compiler doesn't increase the number of additions" prop_noUnneededAdditions
        , testProperty "Error is raised if an undefined variable is used" prop_undefinedVariableError
    ]