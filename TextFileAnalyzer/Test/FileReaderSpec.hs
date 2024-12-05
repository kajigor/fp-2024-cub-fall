module FileReaderSpec (tests) where

import Test.HUnit
import FileReader (readTextFile)
import ErrorHandling (AppError(..))
import System.Directory (doesFileExist)

tests :: Test
tests = TestList
    [ TestLabel "Test Successful File Read" testSuccessfulFileRead
    , TestLabel "Test Non-Existent File" testNonExistentFile
    ]

testSuccessfulFileRead :: Test
testSuccessfulFileRead = TestCase $ do
    let filePath = "test/testfiles/sample.txt"
    fileExists <- doesFileExist filePath
    if not fileExists
       then assertFailure "sample.txt not found. Create test/testfiles/sample.txt with some content."
       else do
           result <- readTextFile filePath
           case result of
               Left _ -> assertFailure "Expected successful file read"
               Right content -> assertBool "Content shouldn't be empty" (not (null content))

testNonExistentFile :: Test
testNonExistentFile = TestCase $ do
    let filePath = "test/testfiles/nonexistent.txt"
    result <- readTextFile filePath
    case result of
        Left (FileReadError _) -> return ()
        Left _ -> assertFailure "Expected FileReadError for non-existent file"
        Right _ -> assertFailure "Expected error, but got successful read"
