import qualified Data.Map.Strict as M

import Expr ( Expr (..) )
import Error ( Error (..) )
import Interpreter ( eval )
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

testEval :: TestTree
testEval =
  testGroup "Eval" [testExpressions, testErrors, testLet, testComplex]
  where
    testExpressions =
      testGroup
        "Test every expression type"
        [ testSuccess "3 == 3" M.empty (Number 3) 3,
          testSuccess "x == 3" (M.singleton "x" 17) (Variable "x") 17,
          testSuccess "sqrt(25) == 5.0" M.empty (Sqrt (Number 25.0)) 5.0,
          testSuccess "1 + 10 == 11" M.empty (Plus (Number 1.0) (Number 10.0)) 11.0,
          testSuccess "2.3 - 4.8 == -2.5" M.empty (Minus (Number 2.3) (Number 4.8)) (-2.5),
          testSuccess "4.0 / 2.5 == 1.6" M.empty (Divide (Number 4.0) (Number 2.5)) 1.6,
          testSuccess "1 / 4.0 == 0.25" M.empty (Divide (Number 1.0) (Number 4.0)) 0.25,
          testSuccess "4.0 * 2.5 == 10" M.empty (Prod (Number 4.0) (Number 2.5)) 10.0,
          testSuccess "2 ^ 10 == 1024" M.empty (Exp (Number 2.0) (Number 10.0)) 1024.0 
        ]
    testErrors = 
      testGroup
        "Test errors"
        [ testFailure "square root of negative number" M.empty testNegativeSquareRoot (RootOfNegative testNegativeSquareRoot),
          testFailure "division by zero" M.empty testDivideByZero (DivisionByZero testDivideByZero),
          testFailure "root of negative number" M.empty testNegativeRoot (RootOfNegative testNegativeRoot),
          testFailure "zero to the negative power" M.empty testZeroToNegative (ZeroToNegativePower testZeroToNegative),
          testFailure "unknown variable" (M.singleton "foo" 3) (Plus (Variable "foo") (Variable "stas")) (UnknownVariable "stas"),
          testFailure "variable already defined" (M.singleton "foo" 3) (Let "foo" (Number 3) (Variable "foo")) (VariableAlreadyDefined "foo") 
        ]
        where
          testNegativeSquareRoot = Sqrt (Prod (Number (-3)) (Number 44))
          testDivideByZero = Divide (Number 5) (Minus (Number 4) (Number 4))
          testNegativeRoot = Exp (Number (-1)) (Divide (Number 1) (Number 2))
          testZeroToNegative = Exp (Number 0) (Minus (Number 0) (Number 10))
    testLet = 
      testGroup 
      "Let Expressions"
        [ testSuccess "let z = 3 in z + 2" M.empty (Let "z" (Number 3.0) (Plus (Variable "z") (Number 2.0))) 5.0,
          testSuccess "let z = 3 in let w = 4 in z + w" M.empty 
            (Let "z" (Number 3.0) (Let "w" (Number 4.0) (Plus (Variable "z") (Variable "w")))) 7.0,
          testFailure "clashing variable let x = 1 in let x = 2 in x" M.empty 
            (Let "x" (Number 1.0) (Let "x" (Number 2.0) (Variable "x"))) (VariableAlreadyDefined "x")
        ]
    testComplex = 
      testGroup 
      "Complex Expressions"
        [ testSuccess "let foo = 25 ^ 0.5 in 2 * (foo + sqrt(16))" M.empty
            (Let "foo" (Exp (Number 25) (Number 0.5)) (Prod (Number 2.0) (Plus (Variable "foo") (Sqrt (Number 16.0))))) 18.0,
          testSuccess "let a = 3 in (a / (2 - 1)) * (a + 2)" M.empty
            (Let "a" (Number 3.0) (Prod (Divide (Variable "a") (Minus (Number 2.0) (Number 1.0))) (Plus (Variable "a") (Number 2.0)))) 15.0,
          testSuccess "let a = 4 in let b = 10 * a + 3 in let c = 10 * b + 2 in let d = 10 * c + 1 in 10 * d" M.empty
            (Let "a" 
              (Number 4)
              (Let "b"
                (Plus (Prod (Number 10) (Variable "a")) (Number 3))
                (Let "c"
                  (Plus (Prod (Number 10) (Variable "b")) (Number 2))
                  (Let "d" 
                    (Plus (Prod (Number 10) (Variable "c")) (Number 1)) (Prod (Number 10) (Variable "d"))
                  )
                ) 
              )
            )
            43210.0
        ]
    testSuccess msg state expr expected =
      testCase msg $
        case eval state expr of
          Right x -> assertBool ("Evaluation result is wrong, should be \n" ++ show expected ++ "\bbut got\n" ++ show x ++ "\n") (x == expected)
          Left err -> assertFailure $ "Expression failed to evaluate: " ++ show err
    testFailure msg state expr expected =
      testCase msg $
        case eval state expr of
          Right _ -> assertFailure "Evaluation result shoul be an error"
          Left err -> assertBool ("Evaluated as a wrong error, should be \n" ++ show expected ++ "\nbut got\n" ++ show err ++ "\n") (err == expected)

main :: IO ()
main =
  defaultMain $ testGroup "Expressions" [testEval]