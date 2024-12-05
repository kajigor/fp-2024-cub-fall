module ErrorHandler (safeReadFile, safeWriteFile) where

import Control.Exception (try, IOException)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Safe file reading
safeReadFile :: FilePath -> IO (Either String T.Text)
safeReadFile path = do
    result <- try (TIO.readFile path) :: IO (Either IOException T.Text)
    return $ case result of
        Left err -> Left (show err)
        Right content -> Right content

-- Safe file writing
safeWriteFile :: FilePath -> T.Text -> IO (Either String ())
safeWriteFile path content = do
    result <- try (TIO.writeFile path content) :: IO (Either IOException ())
    return $ case result of
        Left err -> Left (show err)
        Right () -> Right ()
