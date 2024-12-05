module Main (main) where

import Test.HUnit
import qualified FileReaderSpec
import qualified StatisticsSpec
import qualified FrequentWordsSpec
import qualified NGramAnalysisSpec
import qualified WordCloudSpec
import qualified ErrorHandlingSpec

main :: IO ()
main = do
    _ <- runTestTT $ TestList
        [ FileReaderSpec.tests
        , StatisticsSpec.tests
        , FrequentWordsSpec.tests
        , NGramAnalysisSpec.tests
        , WordCloudSpec.tests
        , ErrorHandlingSpec.tests
        ]
    return ()
