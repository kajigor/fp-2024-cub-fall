module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog (Gen, (===), property, forAll, success, PropertyT, Property, TestLimit, withTests)
import qualified Data.Map as M
import Expr
import Eval
import Parser
import TestUtil (genExpr, genVars)

main :: IO ()
main = defaultMain $ testGroup "all-tests" tests

tests :: [TestTree]
tests =
    [ testGroup "Property" propTests
    , testGroup "Unit" unitTests
    ]

unitTests :: [TestTree]
unitTests = [
    testCase "Eval simple addition" $
        evalExpr M.empty (Plus (Num 1) (Num 2)) @?= Right 3.0

    , testCase "Eval variable lookup" $
        evalExpr (M.fromList [("x", 5)]) (Plus (Var "x") (Num 3)) @?= Right 8.0

    , testCase "Eval division by zero" $
        evalExpr M.empty (Div (Num 1) (Num 0)) @?= Left (EvalError DivByZero (Div (Num 1) (Num 0)))

    , testCase "Parser parses addition correctly" $
        parseExpr "1 + 2" @?= Right (Plus (Num 1) (Num 2))

    , testCase "Parser fails on invalid syntax" $
        case parseExpr "1 +" of
            Left _ -> return ()
            Right _ -> assertFailure "Expected parse error"

    , testCase "Unary minus evaluates correctly" $
        evalExpr M.empty (UnaryMinus (Num 5)) @?= Right (-5.0)

    , testCase "Zero raised to non-positive power" $
        evalExpr M.empty (Pow (Num 0) (Num (-1))) @?= Left (EvalError (ZeroNonPositivePow (-1)) (Pow (Num 0) (Num (-1))))

    , testCase "Unary minus is equivalent to Haskell unary minus" $
        ((Num (-1000.0)), (Num 0.0)) @?= ((UnaryMinus (Num 1000.0)), (Num 0.0))
    ]

-- The number of successful tests that need to be run before a property test is considered successful.
-- 100 tests were often not enough to catch NaN issues, so I increased this number to 1000.
testCount :: TestLimit
testCount = 1000

myProperty :: PropertyT IO () -> Property
myProperty action = withTests testCount $ property action

propTests :: [TestTree]
propTests = [
    testProperty "parseExpr . show == id" $
        myProperty $ do
            (_, vars) <- forAll genVars
            expr <- forAll (genExpr vars)
            parseExpr (show expr) === Right expr

    , testProperty "forall varMap, expr. evalExpr varMap expr == evalExpr varMap (parseExpr (show expr))" $
        myProperty $ do
            (varMap, vars) <- forAll genVars
            expr <- forAll (genExpr vars)
            -- round is used to get rid of NaN
            (round <$> evalExpr varMap expr) === (round <$> evalExpr varMap (either (\e -> error $ show e) id (parseExpr (show expr))))

    , testProperty "evalExpr (e1 + e2) == Right (evalExpr e1) + (evalExpr e2)" $
        myProperty $ do
            (varMap, vars) <- forAll genVars
            e1 <- forAll (genExpr vars)
            e2 <- forAll (genExpr vars)
            case (evalExpr varMap e1, evalExpr varMap e2) of
                (Right v1, Right v2) -> (round <$> evalExpr varMap (Plus e1 e2)) === (round <$> Right (v1 + v2))
                _ -> success

    , testProperty "evalExpr (e1 - e2) == Right (evalExpr e1) - (evalExpr e2)" $
        myProperty $ do
            (varMap, vars) <- forAll genVars
            e1 <- forAll (genExpr vars)
            e2 <- forAll (genExpr vars)
            case (evalExpr varMap e1, evalExpr varMap e2) of
                (Right v1, Right v2) -> (round <$> evalExpr varMap (Minus e1 e2)) === (round <$> Right (v1 - v2))
                _ -> success

    , testProperty "evalExpr (e1 * e2) == Right (evalExpr e1) * (evalExpr e2)" $
        myProperty $ do
            (varMap, vars) <- forAll genVars
            e1 <- forAll (genExpr vars)
            e2 <- forAll (genExpr vars)
            case (evalExpr varMap e1, evalExpr varMap e2) of
                (Right v1, Right v2) -> (round <$> evalExpr varMap (Mult e1 e2)) === (round <$> Right (v1 * v2))
                _ -> success

    , testProperty "evalExpr (e1 / e2) == Right (evalExpr e1) / (evalExpr e2)" $
        myProperty $ do
            (varMap, vars) <- forAll genVars
            e1 <- forAll (genExpr vars)
            e2 <- forAll (genExpr vars)
            case (evalExpr varMap e1, evalExpr varMap e2) of
                (_, Right 0) -> success
                (Right v1, Right v2) -> (round <$> evalExpr varMap (Div e1 e2)) === (round <$> Right (v1 / v2))
                _ -> success
    ]
