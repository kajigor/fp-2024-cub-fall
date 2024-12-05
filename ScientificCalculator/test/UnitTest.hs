module UnitTest where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Either (isLeft)
import Expr
import ParserExpr
import CalcError
import Eval
import Memory
import Control.Monad.State


testValidParser :: String -> String -> Expr -> TestTree
testValidParser name expr expectedResult =
  testCase name $ do
    let parsedExpr = parseInput expr
    assertEqual "Unexpected parsing result for valid input" (Right expectedResult) parsedExpr


testInvalidParser :: String -> String -> TestTree
testInvalidParser name expr =
  testCase name $ do
    let parsedExpr = parseInput expr
    assertBool "Expected a parsing error but got a valid result" (isLeft parsedExpr)


testExprParser :: TestTree
testExprParser = testGroup "Parser Tests"
  [ 

    testValidParser "Addition" "4 + 5" (Add (Num 4) (Num 5)),
    testValidParser "Subtraction" "7 - 3" (Diff (Num 7) (Num 3)),
    testValidParser "Multiplication" "6 * 3" (Mul (Num 6) (Num 3)),
    testValidParser "Division" "8 / 4" (Div (Num 8) (Num 4)),
    testValidParser "Power" "2 ^ 3" (Pow (Num 2) (Num 3)),


    testValidParser "Pi constant" "π" Pi,
    testValidParser "E constant" "e" E,
    testValidParser "Square root" "sqrt(4)" (Sqrt (Num 4)),
    testValidParser "Factorial" "5!" (Factorial (Num 5)),


    testValidParser "Nested addition" "2 + (3 + 4)" 
      (Add (Num 2) (Add (Num 3) (Num 4))),
    testValidParser "Nested multiplication" "2 * (3 + 4)" 
      (Mul (Num 2) (Add (Num 3) (Num 4))),


    testValidParser "Sine" "sin(π / 2)" 
      (Sin (Div Pi (Num 2))),
    testValidParser "Cosine" "cos(0)" 
      (Cos (Num 0)),
    testValidParser "Tangent" "tan(π / 4)" 
      (Tan (Div Pi (Num 4))),


    testInvalidParser "Invalid expression" "4 +",
    testInvalidParser "Unexpected input" "4 4",
    testInvalidParser "Just text" "This is an invalid expression"
  ]


assertAlmostEqual :: String -> Double -> Double -> Double -> Assertion
assertAlmostEqual message expected actual epsilon =
  assertBool (message ++ ": expected " ++ show expected ++ ", but got " ++ show actual) 
    (abs (expected - actual) < epsilon)


testEvalExpression :: String -> Expr -> Double -> TestTree
testEvalExpression name expr expectedResult = 
    testCase name $ do
        let actualResult = eval expr
        case actualResult of
          Right value -> assertAlmostEqual ("Unexpected evaluation result for: " ++ show expr) expectedResult value 1e-9
          Left err    -> assertFailure ("Expected success but got error: " ++ show err)
    


testEvalInvalidExpression :: String -> Expr -> CalcError -> TestTree
testEvalInvalidExpression name expr expectedError = 
    testCase name $ do
        let actualResult = eval expr
        assertEqual ("Unexpected error result for: " ++ show expr) (Left expectedError) actualResult



testEval :: TestTree
testEval = testGroup "Eval Tests"
  [

    testEvalExpression "Addition" (Add (Num 4) (Num 5)) 9,
    testEvalExpression "Subtraction" (Diff (Num 7) (Num 3)) 4,
    testEvalExpression "Multiplication" (Mul (Num 6) (Num 3)) 18,
    testEvalExpression "Division" (Div (Num 8) (Num 4)) 2,
    testEvalExpression "Power" (Pow (Num 2) (Num 3)) 8,


    testEvalExpression "Pi constant" Pi pi,
    testEvalExpression "E constant" E (exp 1),


    testEvalExpression "Square root" (Sqrt (Num 9)) 3,
    testEvalExpression "Factorial" (Factorial (Num 5)) 120,


    testEvalExpression "Nested addition and multiplication" 
      (Add (Num 2) (Mul (Num 3) (Num 4))) 14,


    testEvalExpression "Sine of π/2" (Sin (Div Pi (Num 2))) 1,
    testEvalExpression "Cosine of 0" (Cos (Num 0)) 1,
    testEvalExpression "Tangent of π/4" (Tan (Div Pi (Num 4))) 1,

    testEvalExpression "Complex nested expression" 
      (Add (Mul (Num 2) (Div (Pow (Num 5) (Num 2)) (Num 10))) (Sqrt (Num 16)))
      9,
    testEvalExpression "Combination of constants and functions"
      (Add Pi (Sin (Div Pi (Num 2))))
      (pi + 1),


    testEvalInvalidExpression "Division by zero" (Div (Num 4) (Num 0)) 
      (CalcError DivisionByZero (Div (Num 4) (Num 0))),
    testEvalInvalidExpression "Negative square root" (Sqrt (Num (-1))) 
      (CalcError NegativeSqrt (Sqrt (Num (-1)))),
    testEvalInvalidExpression "Logarithm of zero" (Ln (Num 0)) 
      (CalcError LogNonPositive (Ln (Num 0))),
    testEvalInvalidExpression "Logarithm of negative" (Ln (Num (-5))) 
      (CalcError LogNonPositive (Ln (Num (-5)))),
    testEvalInvalidExpression "Logarithm with invalid base 1" (LogBase (Num 1) (Num 10)) 
      (CalcError InvalidLogBase (LogBase (Num 1) (Num 10))),
    testEvalInvalidExpression "Logarithm with invalid base 0" (LogBase (Num 0) (Num 10)) 
      (CalcError InvalidLogBase (LogBase (Num 0) (Num 10))),
    testEvalInvalidExpression "Logarithm with invalid base -1" (LogBase (Num (-1)) (Num 10)) 
      (CalcError InvalidLogBase (LogBase (Num (-1)) (Num 10))),
    testEvalInvalidExpression "Factorial of negative number" (Factorial (Num (-3))) 
      (CalcError InvalidFactorial (Factorial (Num (-3)))),
    testEvalInvalidExpression "Factorial of non-integer" (Factorial (Num 3.5)) 
      (CalcError InvalidFactorial (Factorial (Num 3.5))),
    testEvalInvalidExpression "Domain error for pow (negative base, fractional exponent)" (Pow (Num (-2)) (Num 0.5)) 
      (CalcError DomainError (Pow (Num (-2)) (Num 0.5))),
    testEvalInvalidExpression "Division of zero by zero" (Div (Num 0) (Num 0)) 
      (CalcError DivisionByZero (Div (Num 0) (Num 0))),
    testEvalInvalidExpression "Arcsine out of domain (> 1)" (ASin (Num 2)) 
      (CalcError DomainError (ASin (Num 2))),
    testEvalInvalidExpression "Arcsine out of domain (< -1)" (ASin (Num (-2))) 
      (CalcError DomainError (ASin (Num (-2)))),
    testEvalInvalidExpression "Arccosine out of domain (> 1)" (ACos (Num 2)) 
      (CalcError DomainError (ACos (Num 2))),
    testEvalInvalidExpression "Arccosine out of domain (< -1)" (ACos (Num (-2))) 
      (CalcError DomainError (ACos (Num (-2)))),
    testEvalInvalidExpression "Arctanh out of domain (>= 1)" (ATanh (Num 1)) 
      (CalcError DomainError (ATanh (Num 1))),
    testEvalInvalidExpression "Arctanh out of domain (<= -1)" (ATanh (Num (-1))) 
      (CalcError DomainError (ATanh (Num (-1)))),
    testEvalInvalidExpression "Hyperbolic arccosine out of domain (< 1)" (ACosh (Num 0)) 
      (CalcError DomainError (ACosh (Num 0))),
    testEvalInvalidExpression "Logarithm with negative argument" (LogBase (Num 10) (Num (-5))) 
      (CalcError LogNonPositive (LogBase (Num 10) (Num (-5))))
  ]


testMemory :: String -> Expr -> MemoryState IO () -> Expr -> TestTree
testMemory name initialState operation expectedState =
  testCase name $ do
    (_, finalState) <- runStateT operation initialState
    assertEqual ("Unexpected memory state for: " ++ name) expectedState finalState


testMemoryResult :: String -> Expr -> TestTree
testMemoryResult name initialState =
  testCase name $ do
    finalState <- evalStateT memoryResult initialState
    assertEqual ("Unexpected memory result for: " ++ name) initialState finalState


testMemoryOperations :: TestTree
testMemoryOperations = testGroup "Memory Operations"
  [
    testMemory "Clear memory" (Num 10) memoryClear (Num 0),
    testMemory "Add to memory" (Num 10) (memoryPlus (Num 5)) (Add (Num 10) (Num 5)),
    testMemory "Subtract from memory" (Num 10) (memoryMinus (Num 3)) (Diff (Num 10) (Num 3)),
    testMemoryResult "Retrieve memory" (Num 10),
    testCase "Memory clear after addition" $ do
      (_, finalState) <- runStateT (memoryPlus (Num 5) >> memoryClear >> memoryResult) (Num 10)
      assertEqual "Unexpected memory state after clearing" (Num 0) finalState
  ]

testStringProcessor :: String -> String -> String -> TestTree
testStringProcessor name str expectedString = 
    testCase name $ do
        let processedString = processString str
        assertEqual ("Unexpected processed string for: " ++ name) expectedString processedString


testProcessedString :: TestTree
testProcessedString = testGroup "Processing String"
    [
        testStringProcessor "String with e" "e^2 + exp 4 + e + deg" "e ^2 + exp 4 + e  + deg",
        testStringProcessor "String with ln" "ln + 4 * 8 + 45" "ln + 4 * 8 + 45",
        testStringProcessor "String with pi and π" "π+5 + pi^2 + pi*8 + pi/2 + π/4" "π +5 + pi ^2 + pi *8 + pi /2 + π /4",
        testStringProcessor "Multiple π" "πππππππππ" "π π π π π π π π π "
    ]



main :: IO ()
main = defaultMain $ testGroup "Calculator Tests"
  [ 
    testExprParser,
    testEval,
    testMemoryOperations,
    testProcessedString
  ]
