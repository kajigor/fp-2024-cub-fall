module ErrorHandling (AppError(..), handleError) where

data AppError
    = FileReadError String
    | InvalidDataError String
    | CommandError String
    deriving (Show, Eq)

handleError :: AppError -> IO ()
handleError (FileReadError msg) = putStrLn $ "File Read Error: " ++ msg
handleError (InvalidDataError msg) = putStrLn $ "Invalid Data Error: " ++ msg
handleError (CommandError msg) = putStrLn $ "Command Error: " ++ msg
