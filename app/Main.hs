{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai.Middleware.Cors (simpleCors)
import Control.Monad.IO.Class (liftIO)
import qualified Sudoku as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types (status400)  -- Import HTTP status codes

main :: IO ()
main = scotty 3000 $ do
    -- Enable CORS for frontend interaction
    middleware simpleCors

    -- Route for generating a new Sudoku puzzle
    get "/generate" $ do
        difficultyStr <- param "difficulty" `rescue` (\_ -> return "easy")
        let difficulty = T.pack difficultyStr  -- Convert String to Text
        liftIO $ putStrLn $ "Generating Sudoku with difficulty: " ++ difficultyStr
        puzzle <- liftIO $ S.generateSudoku difficulty
        liftIO $ putStrLn $ "Generated Sudoku: " ++ show puzzle
        text $ S.encodeGrid puzzle

    -- Route for checking the validity of a Sudoku grid
    post "/check" $ do
        formData <- params
        let strictFormData = map (\(k, v) -> (S.toStrict k, S.toStrict v)) formData
        case S.parseGrid strictFormData of
            Left err -> do
                status status400
                text $ "Error: " <> err
            Right grid -> if S.checkSudoku grid
                          then text "Valid"
                          else do
                              text "Invalid"

    -- Route for solving a Sudoku puzzle
    post "/solve" $ do
        formData <- params
        let strictFormData = map (\(k, v) -> (S.toStrict k, S.toStrict v)) formData
        case S.parseGrid strictFormData of
            Left err -> do
                status status400
                text $ "Error: " <> err
            Right grid -> do
                result <- liftIO $ S.solveSudoku grid
                case result of
                    Left errMsg -> do
                        status status400
                        text $ "Error: " <> TL.pack errMsg
                    Right solvedGrid -> do
                        liftIO $ putStrLn $ "Solved Sudoku: " ++ show solvedGrid
                        text $ S.encodeGrid solvedGrid
