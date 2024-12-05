module Logic where
import Game
import Graphics.Gloss.Interface.Pure.Game
import Data.Array

playerTurn :: Game -> (Int, Int) -> Game

isWithinBounds :: (Int, Int) -> Int -> Bool
isWithinBounds (x, y) n = x >= 0 && x < n && y >= 0 && y < n


safeAccess :: Board -> Int -> (Int, Int) -> Cell
safeAccess board  n (x, y)
    | isWithinBounds (x, y) n = board ! (x, y)
    | otherwise = Nothing

isDirectionValid :: Game -> (Int, Int) -> (Int, Int) -> Bool

isDirectionValid game (x, y) (dx, dy) = (any (== Just (player game)) $ takeWhile (\f -> f /= Nothing) $ tail $ map (\k -> safeAccess board (n game) (x + k * dx, y + k * dy)) [1..(n game)]) && (safeAccess board (n game) (x + dx, y + dy) == Just (oppositePlayer game))
    where board = gameBoard game 

isPlaceValid :: Game -> (Int, Int) -> Bool

isPlaceValid game pos = any (isDirectionValid game pos) [(dx, dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0)]

flipCells :: Game -> (Int, Int) -> Game

lengthDirection :: Game -> Player -> (Int, Int) -> (Int, Int) -> Int

lengthDirection game player (x, y) (dx, dy) = length $ takeWhile (\f -> f == Just (player)) $ map (\k -> safeAccess board (n game) (x + k * dx, y + k * dy)) [1..(n game)]
    where board = gameBoard game
flipCells game (x, y)
    | isPlaceValid game (x, y) && isWithinBounds (x, y) (n game) = game {gameBoard = newBoard}
    | otherwise = game
    where 
        board = gameBoard game
        recPlayer = oppositePlayer game
        directions = [(dx, dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0)]
        validDirections = filter (isDirectionValid game (x, y)) directions
        newBoard = foldl (flipCoins) board validDirections
        flipCoins :: Board -> (Int, Int) -> Board
        flipCoins board (dx, dy) = board // [((x, y), Just (player game))] //  map (\f -> ((x + f * dx, y + f * dy), Just (player game))) [1..(lengthDirection game (recPlayer) (x, y) (dx, dy))]

switchPlayer :: Game -> Game
switchPlayer game = case player game of
    Player1 -> game { player = Player2 }
    Player2 -> game { player = Player1 }

isThereMoves :: Game -> Bool
isThereMoves game = any (== True) $ map (\(x, y) -> board ! (x, y) == Nothing && isPlaceValid game (x, y)) [(x, y) | x <- [0..bound-1], y <- [0..bound-1]]
    where board = gameBoard game
          bound = n game

checkEnding :: Game -> Game
checkEnding game = case ((length $ filter (== Nothing) $ elems $ gameBoard game) == 0 || isThereMoves game == False) of
    True -> game { state = GameOver (winner game) }
    _ -> game
    where winner game
            | length (filter (== Just Player1) $ elems $ gameBoard game) > length (filter (== Just Player2) $ elems $ gameBoard game) = Just Player1
            | length (filter (== Just Player1) $ elems $ gameBoard game) < length (filter (== Just Player2) $ elems $ gameBoard game) = Just Player2
            | otherwise = Nothing

playerTurn game (x, y)
    | isWithinBounds(x, y) (n game) && isPlaceValid game (x, y) && board ! (x, y) == Nothing = 
        
        checkEnding $ switchPlayer $ flipCells (game { gameBoard = ((board) // [((x, y), Just (player game))]) }) (x, y)
    | otherwise = game
    where board = gameBoard game

translateToCell :: (Float, Float) -> Game -> (Int, Int)
translateToCell (x, y) game = ( floor ((y + (fromIntegral (screenHeight game)* 0.5)) / (cellHeight game))
                         , floor ((x + (fromIntegral (screenWidth game) * 0.5)) / (cellWidth game))
                    )

transformGame :: Event -> Game -> Game
transformGame (EventKey (MouseButton LeftButton) Up _ (x, y)) game =
    case state game of
        Running -> checkEnding $ playerTurn game $ translateToCell (x,y) game
        GameOver _ ->  game
transformGame _ game = game 