module TestUtil where

import Hedgehog (Gen, (===), property, forAll, success, PropertyT, Property, TestLimit, withTests)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.Map as M
import Expr

genExpr :: [String] -> Gen Expr
genExpr vars = Gen.recursive Gen.choice
    [
        Num <$> Gen.double (Range.linearFrac (-1000) 1000),
        if null vars
            then Gen.constant (Num 0)
            else Var <$> Gen.element vars
    ]
    [
        Plus <$> genExpr vars <*> genExpr vars,
        Minus <$> genExpr vars <*> genExpr vars,
        Mult <$> genExpr vars <*> genExpr vars,
        Div <$> genExpr vars <*> genExpr vars,
        Pow <$> genExpr vars <*> genExpr vars,
        Abs <$> genExpr vars,
        UnaryMinus <$> genExpr vars
    ]

genVars :: Gen (M.Map String Double, [String])
genVars = do
    vars <- Gen.list (Range.linear 0 10) genVar
    let varMap = M.fromList vars
    let varNames = map fst vars
    return (varMap, varNames)
    where
        genVar = do
            name <- Gen.string (Range.linear 1 5) Gen.alpha
            value <- Gen.double (Range.linearFrac (-1000) 1000)
            return (name, value)
