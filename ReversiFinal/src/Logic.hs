module Logic where
import Game
import Graphics.Gloss.Interface.Pure.Game
import Data.Array

playerTurn :: Game -> (Int, Int) -> Game

isWithinBounds :: (Int, Int) -> Bool
isWithinBounds (x, y) = x >= 0 && x < n && y >= 0 && y < n


safeAccess :: Board -> (Int, Int) -> Cell
safeAccess board (x, y)
    | isWithinBounds (x, y) = board ! (x, y)
    | otherwise = Nothing

isDirectionValid :: Game -> (Int, Int) -> (Int, Int) -> Bool

isDirectionValid game (x, y) (dx, dy) = (any (== Just (player game)) $ takeWhile (\f -> f /= Nothing) $ tail $ map (\k -> safeAccess board (x + k * dx, y + k * dy)) [1..n]) && (safeAccess board (x + dx, y + dy) == Just (oppositePlayer game))
    where board = gameBoard game

isPlaceValid :: Game -> (Int, Int) -> Bool

isPlaceValid game pos = any (isDirectionValid game pos) [(dx, dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0)]

flipCells :: Game -> (Int, Int) -> Game

lengthDirection :: Board -> Player -> (Int, Int) -> (Int, Int) -> Int

lengthDirection board player (x, y) (dx, dy) = length $ takeWhile (\f -> f == Just (player)) $ map (\k -> safeAccess board (x + k * dx, y + k * dy)) [1..n]

flipCells game (x, y)
    | isPlaceValid game (x, y) && isWithinBounds (x, y) = game {gameBoard = newBoard}
    | otherwise = game
    where 
        board = gameBoard game
        recPlayer = oppositePlayer game
        directions = [(dx, dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0)]
        validDirections = filter (isDirectionValid game (x, y)) directions
        newBoard = foldl (flipCoins) board validDirections
        flipCoins :: Board -> (Int, Int) -> Board
        flipCoins board (dx, dy) = board // [((x, y), Just (player game))] //  map (\f -> ((x + f * dx, y + f * dy), Just (player game))) [1..(lengthDirection board (recPlayer) (x, y) (dx, dy))]

switchPlayer :: Game -> Game
switchPlayer game = case player game of
    Player1 -> game { player = Player2 }
    Player2 -> game { player = Player1 }

checkEnding :: Game -> Game
checkEnding game = case (length $ filter (== Nothing) $ elems $ gameBoard game) of
    0 -> game { state = GameOver (winner game) }
    _ -> game
    where winner game
            | length (filter (== Just Player1) $ elems $ gameBoard game) > length (filter (== Just Player2) $ elems $ gameBoard game) = Just Player1
            | length (filter (== Just Player1) $ elems $ gameBoard game) < length (filter (== Just Player2) $ elems $ gameBoard game) = Just Player2
            | otherwise = Nothing

playerTurn game (x, y)
    | isWithinBounds(x, y) && isPlaceValid game (x, y) && board ! (x, y) == Nothing = 
        
        checkEnding $ switchPlayer $ flipCells (game { gameBoard = ((board) // [((x, y), Just (player game))]) }) (x, y)
    | otherwise = game
    where board = gameBoard game

translateToCell :: (Float, Float) -> (Int, Int)
translateToCell (x, y) = ( floor ((y + (fromIntegral screenHeight * 0.5)) / cellHeight)
                         , floor ((x + (fromIntegral screenWidth * 0.5)) / cellWidth)
                    )

transformGame :: Event -> Game -> Game
transformGame (EventKey (MouseButton LeftButton) Up _ (x, y)) game =
    case state game of
        Running -> playerTurn game $ translateToCell (x,y) 
        GameOver _ -> game
    where board = gameBoard game
transformGame _ game = game 