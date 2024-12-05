module Main where

import Criterion.Main
import FileReader (readTextFile)
import Statistics (computeStatistics, TextStatistics(..))
import NGramAnalysis (generateNGrams)
import System.IO.Temp (withSystemTempFile)
import System.IO (hPutStr, hClose)

generateSampleText :: Int -> String
generateSampleText n = unlines $ replicate n "i love jetbrains and bremen"

main :: IO ()
main = do
    let sampleText = generateSampleText 10

    withSystemTempFile "sample.txt" $ \filePath handle -> do
        hPutStr handle sampleText
        hClose handle

        defaultMain
            [ bgroup "FileReader"
                [ bench "readTextFile" $ whnfIO (readTextFile filePath)
                ]
            , bgroup "Statistics"
                [ bench "computeStatistics" $ whnf computeStatistics sampleText
                ]
            , bgroup "NGramAnalysis"
                [ bench "generateNGrams 3" $ whnf (generateNGrams 3) sampleText
                ]
            ]
