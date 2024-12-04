module Board where

data Disc = Black | White deriving (Eq, Show)

type Board = [[Maybe Disc]]

data GameState = GameState
  { board :: Board,
    currentPlayer :: Disc,
    history :: [Board]
  }
  deriving (Show)

initBoard :: Int -> Board
initBoard size =
  foldl
    (\board (pos, val) -> updateBoard board pos val)
    emptyBoard
    [ ((mid, mid), Just White)
    , ((mid - 1, mid - 1), Just White)
    , ((mid, mid - 1), Just Black)
    , ((mid - 1, mid), Just Black)
    ]
  where
    mid = size `div` 2
    emptyBoard = replicate size (replicate size Nothing)

updateBoard :: Board -> (Int, Int) -> Maybe Disc -> Board
updateBoard board (x, y) val =
  take x board ++ [take y (board !! x) ++ [val] ++ drop (y + 1) (board !! x)] ++ drop (x + 1) board

initGame :: Int -> GameState
initGame size = GameState (initBoard size) Black []
