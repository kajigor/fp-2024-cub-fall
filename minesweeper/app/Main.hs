module Main where

import Grid.Core (renderGrid)
import Grid.Generation (generateEmptyGrid)
import Game (askFirstMove, gameLoop, getGameParameters, checkWin)

main = do
    paramsResult <- getGameParameters
    case paramsResult of
        Left err -> do
            putStrLn $ "Error: " ++ err
            main  -- Restart if parameters are invalid
        Right (rows, cols, mines) -> do
            let emptyGrid = generateEmptyGrid rows cols
            firstMoveResult <- askFirstMove emptyGrid rows cols mines
            case firstMoveResult of
                Left err -> do
                    putStrLn $ "Error: " ++ err
                    main  -- Restart if first move is invalid
                Right grid -> do
                    -- Check for win condition after the first move
                    if checkWin grid
                        then do
                            putStrLn "\nCongratulations! You cleared the grid!"
                            renderGrid grid
                        else gameLoop grid rows cols