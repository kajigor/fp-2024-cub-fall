module Main where

import Grid.Generation (generateEmptyGrid)
import Game (askFirstMove, gameLoop, getGameParameters)

main :: IO ()
main = do
    result <- getGameParameters
    case result of
        Left err -> do
            putStrLn $ "Error: " ++ err
            main
        Right (rows, cols, mines) -> do
            let emptyGrid = generateEmptyGrid rows cols
            firstMoveResult <- askFirstMove emptyGrid rows cols mines
            case firstMoveResult of
                Left err -> do
                    putStrLn $ "Error: " ++ err
                    main
                Right (_, _, grid) -> gameLoop grid rows cols