module Statistics (TextStatistics(..), computeStatistics, displayStatistics) where

data TextStatistics = TextStatistics
    { lineCount     :: Int
    , wordCount     :: Int
    , characterCount :: Int
    } deriving (Show, Eq)

computeStatistics :: String -> TextStatistics
computeStatistics content = TextStatistics
    { lineCount = length (lines content)
    , wordCount = length (words content)
    , characterCount = length content
    }

displayStatistics :: TextStatistics -> IO ()
displayStatistics stats = do
    putStrLn $ "Lines: " ++ show (lineCount stats)
    putStrLn $ "Words: " ++ show (wordCount stats)
    putStrLn $ "Characters: " ++ show (characterCount stats)
