module ErrorHandlingSpec (tests) where

import Test.HUnit
import ErrorHandling (AppError(..), handleError)
import System.IO.Silently (capture_)

tests :: Test
tests = TestList
    [ TestLabel "Test File Read Error (Nonexistent File)" testFileReadErrorNonexistent
    , TestLabel "Test File Read Error (Permission Denied)" testFileReadErrorPermission
    ]

testFileReadErrorNonexistent :: Test
testFileReadErrorNonexistent = TestCase $ do
    let err = FileReadError "nonexistent.txt"
    output <- capture_ (handleError err)
    assertBool "Should contain 'File Read Error: nonexistent.txt'"
        ("File Read Error: nonexistent.txt" `elem` lines output)

testFileReadErrorPermission :: Test
testFileReadErrorPermission = TestCase $ do
    let err = FileReadError "Permission denied"
    output <- capture_ (handleError err)
    assertBool "Should contain 'File Read Error: Permission denied'"
        ("File Read Error: Permission denied" `elem` lines output)
