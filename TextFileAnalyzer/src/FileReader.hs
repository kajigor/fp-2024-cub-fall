module FileReader (readTextFile) where

import ErrorHandling (AppError(..))
import Control.Exception (try, IOException)
import System.IO.Error (ioeGetErrorString)

readTextFile :: FilePath -> IO (Either AppError String)
readTextFile path = do
    result <- try (readFile path) :: IO (Either IOException String)
    case result of
        Left ex -> return $ Left $ FileReadError (ioeGetErrorString ex)
        Right content -> return $ Right content
