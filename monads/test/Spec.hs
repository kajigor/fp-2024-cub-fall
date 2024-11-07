import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified FailCont.Main as FCMain
import FailCont.FailCont

main = defaultMain tests

tests :: TestTree
tests = testGroup "tests"
    [
        testGroup "add"
            [
                test "13+42=55" (FCMain.addInts "13" "42") (Right 55),
                test "EmptyInput" (FCMain.addInts "" "42") (Left FCMain.EmptyInput),
                test "EmptyInput" (FCMain.addInts "13" "forty two") (Left $ FCMain.ParseFailed "forty two")
            ],
        testGroup "div"
            [
                test "EmptyInput" (FCMain.divInts "13" "42") (Right 0),
                test "EmptyInput" (FCMain.divInts "42" "13") (Right 3),
                test "EmptyInput" (FCMain.divInts "13" "0") (Left FCMain.DivisionByZero),
                test "EmptyInput" (FCMain.divInts "13" "000") (Left FCMain.DivisionByZero),
                test "EmptyInput" (FCMain.divInts "0" "0") (Left FCMain.DivisionByZero),
                test "EmptyInput" (FCMain.divInts "" "42") (Left FCMain.EmptyInput)
            ],
        testGroup "gaussian"
            [
                test "EmptyInput" (FCMain.checkGaussianPrime "13" "42") (Right True),
                test "EmptyInput" (FCMain.checkGaussianPrime "0" "0") (Right False),
                test "EmptyInput" (FCMain.checkGaussianPrime "0" "3") (Right True),
                test "EmptyInput" (FCMain.checkGaussianPrime "" "42") (Left FCMain.EmptyInput)
            ]
    ] where
        test name failContExpr expected = testCase name $ evalFailCont failContExpr @?= expected
