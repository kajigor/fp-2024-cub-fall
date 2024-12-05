import qualified UnitTest as U

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import ParserExpr
import qualified Eval
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)
import ApplyFunctions



genNumber :: Int -> Int -> Gen String
genNumber a b = show <$> Gen.realFrac_ (Range.linearFrac (fromIntegral a) (fromIntegral b))

genConstant :: Gen String
genConstant = Gen.element ["pi", "e", "π"]


nonRestrictedBinary = [ "+", "-", "*", "EE" ]

restrictedBinary = ["/", "^", "Lny"]

genOperator :: [String] -> Gen String
genOperator = Gen.element

nonRestrictedUnary = 
  [ 
    "sin", "cos", "tan", "exp", "sinh", "cosh", 
    "tanh", "deg", "rad", "cbrt" , "x^2", "x^3"
  ]

restrictedUnary =  
  [ 
		"sqrt", "ln", "log10", "asin", "acos", 
		"atan", "asinh", "acosh", "atanh",  "1/x"
	]

genUnaryFunction :: [String] -> Gen String
genUnaryFunction = Gen.element 

genSimpleExpr :: Int -> Int -> Gen String
genSimpleExpr a b = Gen.choice [genNumber a b, genConstant]

genAllExpr :: Int -> Gen String
genAllExpr 0 = genSimpleExpr (-1000) 1000
genAllExpr n = Gen.choice
  [ 
		genSimpleExpr (-1000) 1000
  , do
      func <- genUnaryFunction (nonRestrictedUnary ++ restrictedUnary)
      expr <- genAllExpr (n - 1)
      return $ func ++ "(" ++ expr ++ ")"
  , do
      expr1 <- genAllExpr (n `div` 2)
      expr2 <- genAllExpr (n `div` 2)
      op <- genOperator (nonRestrictedBinary ++ restrictedBinary)
      return $ "(" ++ expr1 ++ " " ++ op ++ " " ++ expr2 ++ ")"
  ]

processScientificNotation :: String -> String
processScientificNotation [] = []
processScientificNotation ('e':sign:rest) 
  | sign == '-' || sign == '+' = " EE " ++ [sign] ++ processScientificNotation rest
processScientificNotation (c:rest) = c : processScientificNotation rest

genNonRestrictedExpr :: Int -> Gen (String, Double)
genNonRestrictedExpr 0 = do
  num <- Gen.double (Range.linearFrac (-1000) 1000)
  return (processScientificNotation $ show num, num)
genNonRestrictedExpr n = Gen.choice
  [ do
      (exprStr, value) <- genNonRestrictedExpr 0
      return (exprStr, value)
  , do
      func <- genUnaryFunction nonRestrictedUnary
      (exprStr, value) <- genNonRestrictedExpr (n - 1)
      let result = applyUnary func value
      return (func ++ "(" ++ exprStr ++ ")", result)
  , do
      (expr1Str, value1) <- genNonRestrictedExpr (n `div` 2)
      (expr2Str, value2) <- genNonRestrictedExpr (n `div` 2)
      op <- genOperator nonRestrictedBinary
      let result = applyBinary op value1 value2
      return ("(" ++ expr1Str ++ " " ++ op ++ " " ++ expr2Str ++ ")", result)
  ]

dynamicEpsilon :: Double -> Double
dynamicEpsilon expectedValue = max 1e-6 (abs expectedValue * 1e-9)


relativeError :: Double -> Double -> Double
relativeError expected actual = abs (expected - actual) / max (abs expected) 1e-9


prop_successOnNonRestrictedExpr :: Property
prop_successOnNonRestrictedExpr = property $ do
    (exprStr, expectedValue) <- forAll (genNonRestrictedExpr 4)
    case parseInput exprStr of
        Left err -> footnote ("Parse failed: " ++ show err) >> failure
        Right expr -> case Eval.eval expr of
            Left calcErr -> footnote ("Evaluation Failed: " ++ show calcErr) >> failure
            Right actualValue
                | isNaN expectedValue && isNaN actualValue -> success
                | isInfinite expectedValue && isInfinite actualValue
                  && (expectedValue > 0) == (actualValue > 0) -> success 
                | abs (expectedValue - actualValue) < dynamicEpsilon expectedValue -> success 
                | relativeError expectedValue actualValue < 1e-9 -> success
                | otherwise ->
                    footnote ("Expected: " ++ show expectedValue ++ ", but got: " ++ show actualValue) >> failure

prop_validExpressionsParse :: Property
prop_validExpressionsParse = property $ do
  expr <- forAll (genAllExpr 10) 
  case parseInput expr of
    Left err -> footnote ("Parse failed: " ++ show err) >> failure
    Right _  -> success


prop_precisionEval :: Property
prop_precisionEval = property $ do
  (exprStr, expectedValue) <- forAll $ Gen.element 
    [("0.1 + 0.2", 0.3), ("1 EE -9 * 1 EE 9", 1)]
  case parseInput exprStr of
    Left err -> footnote ("Parse failed: " ++ show err) >> failure
    Right expr -> case Eval.eval expr of
      Left calcErr -> footnote ("Evaluation Failed" ++ show calcErr) >> failure
      Right actualValue -> Hedgehog.assert $ abs (expectedValue - actualValue) < 1e-9


main :: IO ()
main = defaultMain $ testGroup "Scientific Calculator Test"
  [ 
    testProperty "Hedgehog consistent parsing" prop_validExpressionsParse,
		testProperty "Hedgehog consistent eval for non-restricted operations" prop_successOnNonRestrictedExpr,
		testProperty "Hedgehog precision" prop_precisionEval,
    U.testExprParser,
    U.testEval,
    U.testMemoryOperations,
    U.testProcessedString
  ]
