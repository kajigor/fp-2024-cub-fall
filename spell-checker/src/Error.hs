module Error
  ( handleFileError
  , validateInput
  , reportError
  ) where

import System.IO.Error (catchIOError, ioeGetErrorString)
import Control.Exception (SomeException, catch)

-- Handle file errors and provide fallback actions
handleFileError :: IO a -> IO (Maybe a)
handleFileError action = catchIOError
  (Just <$> action)
  (\err -> do
      putStrLn $ "File error: " ++ ioeGetErrorString err
      return Nothing
  )

-- Validate user input based on a condition
validateInput :: (String -> Bool) -> String -> IO String
validateInput isValid prompt = do
    putStrLn prompt
    input <- getLine
    if isValid input
      then return input
      else do
        putStrLn "Invalid input. Please try again."
        validateInput isValid prompt

-- Report general errors during IO operations
reportError :: IO a -> IO (Either String a)
reportError action = catch
  (Right <$> action)
  (\err -> do
      let errorMessage = show (err :: SomeException)
      putStrLn $ "Error: " ++ errorMessage
      return $ Left errorMessage
  )
