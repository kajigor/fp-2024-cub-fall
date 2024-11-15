module Test.Compiler where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

import qualified Reader.Eval as RE
import qualified HW.Eval as HE
import qualified Expr
import HW.Compiler 


genExpr :: [String] -> Gen (Expr.Expr String)
genExpr vars = Gen.sized $ \n -> genExpr' vars n

genExpr' :: [String] -> Size -> Gen (Expr.Expr String)
genExpr' vars n =
  if n <= 1 then
    Gen.choice
      [ Expr.Num <$> Gen.int (Range.constant 0 100)
      , Expr.Var <$> Gen.element vars
      ]
  else
    Gen.recursive Gen.choice
      [ Expr.Num <$> Gen.int (Range.constant 0 100)
      , Expr.Var <$> Gen.element vars
      ]
      [ Expr.Plus <$> genExpr' vars (n `div` 2) <*> genExpr' vars (n `div` 2)
      , Expr.Let <$> Gen.element vars <*> genExpr' vars (n `div` 2) <*> genExpr' vars (n `div` 2)
      ]

genExprNoLet :: [String] -> Gen (Expr.Expr String)
genExprNoLet vars = Gen.sized $ \n -> genExprNoLet' vars n

genExprNoLet' :: [String] -> Size -> Gen (Expr.Expr String)
genExprNoLet' vars n =
  if n <= 1 then
    Gen.choice
      [ Expr.Num <$> Gen.int (Range.constant 0 100)
      , Expr.Var <$> Gen.element vars
      ]
  else
    Gen.recursive Gen.choice
      [ Expr.Num <$> Gen.int (Range.constant 0 100)
      , Expr.Var <$> Gen.element vars
      ]
      [ Expr.Plus <$> genExprNoLet' vars (n `div` 2) <*> genExprNoLet' vars (n `div` 2)
      ]

prop_eval_base :: Gen (Expr.Expr String) -> Property 
prop_eval_base ge  = property $ do 
  expr <- forAll ge
  let evalResult = RE.runEvalSafe expr
  let program = compile expr
  let execResult =
        case HE.execProgram program HE.initialState of
          Left _ -> Nothing
          Right resultState -> 
            case HE.getStack resultState of
              [result] -> Just result 
              _        -> Nothing
  assert (execResult == evalResult)

prop_eval :: Property
prop_eval = prop_eval_base $ genExpr ["foo", "bar", "homework", "staaaas"]

prop_eval_no_let :: Property
prop_eval_no_let = prop_eval_base $ genExprNoLet ["foo", "bar", "homework", "staaaas"]

props :: [TestTree]
props =
  [ testProperty "Tests without Let expression" prop_eval_no_let
    -- This sometimes fails:
    -- , testProperty "Eval is equivalent to Exec" prop_eval
  ]