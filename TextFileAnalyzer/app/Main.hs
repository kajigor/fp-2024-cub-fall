module Main where

import System.Environment (getArgs)
import FileReader (readTextFile)
import ErrorHandling (handleError)
import Interface (mainMenu)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> processFile filePath
        _          -> putStrLn "Usage: text-file-analyzer <file-path>"


processFile :: FilePath -> IO ()
processFile filePath = do
    result <- readTextFile filePath
    case result of
        Left err -> handleError err
        Right content -> do
            mainMenu content
