import ConsoleUI
import GameLogic
import Board

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.HUnit
import Test.Tasty

checkValidMove :: Either MoveError GameState -> String -> IO GameState
checkValidMove (Left err) _ = fail $ "Error: " ++ show err
checkValidMove (Right gameState) expectedOutput = do
  renderBoard (board gameState) @?= expectedOutput
  return gameState

checkInvalidMove :: Either MoveError GameState -> MoveError -> IO ()
checkInvalidMove (Left err) expectedError = err @?= expectedError
checkInvalidMove (Right _) _ = fail "Expected an invalid move error, but the move was successful."

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Reversi Tests"
  [ unitTests
  , propertyTests
  ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testCase "Valid move" $
      case isValidMove (initGame 8) (3, 2) of
        Left err -> fail ("Expected valid move, but got error: " ++ show err)
        Right _ -> return ()

  , testCase "Invalid move on board" $
      case isValidMove (initGame 8) (0, 0) of
        Left CannotFlip -> return ()
        Left err -> fail ("Unexpected error: " ++ show err)
        Right _ -> fail "Move should be invalid"

  , testCase "Invalid move outside board" $ do
      case isValidMove (initGame 8) (-1, 8) of
        Left OutOfBounds -> return ()
        Left err -> fail ("Unexpected error: " ++ show err)
        Right _ -> fail "Move should be invalid"

  , testCase "Initial board contains correct pieces" $ do
      let curBoard = initBoard 8
      curBoard !! 3 !! 3 @?= Just White
      curBoard !! 4 !! 4 @?= Just White
      curBoard !! 3 !! 4 @?= Just Black
      curBoard !! 4 !! 3 @?= Just Black

  , testCase "Update board correctly changes a position" $ do
      let updatedBoard = updateBoard (initBoard 8) (2, 3) (Just Black)
      updatedBoard !! 2 !! 3 @?= Just Black

  , testCase "Move flips opponent's pieces" $ do
      case applyMove (initGame 8) (3, 2) of
        Left err -> fail (show err)
        Right updatedGame -> do
          board updatedGame !! 3 !! 3 @?= Just Black

  , testCase "Test whole game" $ do
      let curBoard = initGame 8
      curBoard1 <- checkValidMove (applyMove curBoard (3, 2)) $ unlines  -- B: 4C
        [ "  A B C D E F G H"
        , "1 . . . . . . . ."
        , "2 . . . . . . . ."
        , "3 . . . . . . . ."
        , "4 . . B B B . . ."
        , "5 . . . B W . . ."
        , "6 . . . . . . . ."
        , "7 . . . . . . . ."
        , "8 . . . . . . . ."
        ]

      checkInvalidMove (applyMove curBoard1 (100, 100)) OutOfBounds

      checkInvalidMove (applyMove curBoard1 (3, 2)) PositionOccupied  -- 4C

      checkInvalidMove (applyMove curBoard1 (0, 0)) CannotFlip  -- 1A

      curBoard2 <- checkValidMove (applyMove curBoard1 (2, 2)) $ unlines  -- W: 3C
        [ "  A B C D E F G H"
        , "1 . . . . . . . ."
        , "2 . . . . . . . ."
        , "3 . . W . . . . ."
        , "4 . . B W B . . ."
        , "5 . . . B W . . ."
        , "6 . . . . . . . ."
        , "7 . . . . . . . ."
        , "8 . . . . . . . ."
        ]

      curBoard3 <- checkValidMove (applyMove curBoard2 (1, 2)) $ unlines  -- B: 2C
        [ "  A B C D E F G H"
        , "1 . . . . . . . ."
        , "2 . . B . . . . ."
        , "3 . . B . . . . ."
        , "4 . . B W B . . ."
        , "5 . . . B W . . ."
        , "6 . . . . . . . ."
        , "7 . . . . . . . ."
        , "8 . . . . . . . ."
        ]

      curBoard4 <- checkValidMove (applyMove curBoard3 (1, 1)) $ unlines  -- W: 2B
        [ "  A B C D E F G H"
        , "1 . . . . . . . ."
        , "2 . W B . . . . ."
        , "3 . . W . . . . ."
        , "4 . . B W B . . ."
        , "5 . . . B W . . ."
        , "6 . . . . . . . ."
        , "7 . . . . . . . ."
        , "8 . . . . . . . ."
        ]

      curBoard5 <- checkValidMove (applyMove curBoard4 (1, 0)) $ unlines  -- B: 2A
        [ "  A B C D E F G H"
        , "1 . . . . . . . ."
        , "2 B B B . . . . ."
        , "3 . . W . . . . ."
        , "4 . . B W B . . ."
        , "5 . . . B W . . ."
        , "6 . . . . . . . ."
        , "7 . . . . . . . ."
        , "8 . . . . . . . ."
        ]

      curBoard6 <- checkValidMove (applyMove curBoard5 (4, 2)) $ unlines  -- W: 5C
        [ "  A B C D E F G H"
        , "1 . . . . . . . ."
        , "2 B B B . . . . ."
        , "3 . . W . . . . ."
        , "4 . . W W B . . ."
        , "5 . . W W W . . ."
        , "6 . . . . . . . ."
        , "7 . . . . . . . ."
        , "8 . . . . . . . ."
        ]

      curBoard7 <- checkValidMove (applyMove curBoard6 (5, 2)) $ unlines  -- B: 6C
        [ "  A B C D E F G H"
        , "1 . . . . . . . ."
        , "2 B B B . . . . ."
        , "3 . . B . . . . ."
        , "4 . . B W B . . ."
        , "5 . . B B W . . ."
        , "6 . . B . . . . ."
        , "7 . . . . . . . ."
        , "8 . . . . . . . ."
        ]

      return ()

  , testCase "Test another game" $ do
      let curBoard = initGame 8
      curBoard1 <- checkValidMove (applyMove curBoard (2, 3)) $ unlines  -- B: 3D
        [ "  A B C D E F G H"
        , "1 . . . . . . . ."
        , "2 . . . . . . . ."
        , "3 . . . B . . . ."
        , "4 . . . B B . . ."
        , "5 . . . B W . . ."
        , "6 . . . . . . . ."
        , "7 . . . . . . . ."
        , "8 . . . . . . . ."
        ]

      curBoard2 <- checkValidMove (applyMove curBoard1 (2, 2)) $ unlines  -- W: 3C
        [ "  A B C D E F G H"
        , "1 . . . . . . . ."
        , "2 . . . . . . . ."
        , "3 . . W B . . . ."
        , "4 . . . W B . . ."
        , "5 . . . B W . . ."
        , "6 . . . . . . . ."
        , "7 . . . . . . . ."
        , "8 . . . . . . . ."
        ]

      curBoard3 <- checkValidMove (applyMove curBoard2 (3, 2)) $ unlines  -- B: 4C
        [ "  A B C D E F G H"
        , "1 . . . . . . . ."
        , "2 . . . . . . . ."
        , "3 . . W B . . . ."
        , "4 . . B B B . . ."
        , "5 . . . B W . . ."
        , "6 . . . . . . . ."
        , "7 . . . . . . . ."
        , "8 . . . . . . . ."
        ]

      curBoard4 <- checkValidMove (applyMove curBoard3 (4, 2)) $ unlines  -- W: 5C
        [ "  A B C D E F G H"
        , "1 . . . . . . . ."
        , "2 . . . . . . . ."
        , "3 . . W B . . . ."
        , "4 . . W B B . . ."
        , "5 . . W W W . . ."
        , "6 . . . . . . . ."
        , "7 . . . . . . . ."
        , "8 . . . . . . . ."
        ]


      curBoard5 <- checkValidMove (applyMove curBoard4 (5, 1)) $ unlines  -- B: 6B
        [ "  A B C D E F G H"
        , "1 . . . . . . . ."
        , "2 . . . . . . . ."
        , "3 . . W B . . . ."
        , "4 . . W B B . . ."
        , "5 . . B W W . . ."
        , "6 . B . . . . . ."
        , "7 . . . . . . . ."
        , "8 . . . . . . . ."
        ]

      curBoard6 <- checkValidMove (applyMove curBoard5 (2, 4)) $ unlines  -- W: 3E
        [ "  A B C D E F G H"
        , "1 . . . . . . . ."
        , "2 . . . . . . . ."
        , "3 . . W W W . . ."
        , "4 . . W B W . . ."
        , "5 . . B W W . . ."
        , "6 . B . . . . . ."
        , "7 . . . . . . . ."
        , "8 . . . . . . . ."
        ]

      return ()
  ]

propertyTests :: TestTree
propertyTests = testGroup "Property-Based Tests"
  [ testProperty "Initial board has exactly 4 pieces" prop_initialBoardHasFourPieces
  , testProperty "Current player alternates" prop_currentPlayerAlternates
  , testProperty "Valid moves are inbounds" prop_validMoveIsWithinBounds
  ]

validMoves :: Board -> Disc -> [(Int, Int)]
validMoves board player = [(x, y) | x <- [0..size-1], y <- [0..size-1], isValidMove (GameState board player []) (x, y) == Right ()]
  where
    size = length board

prop_initialBoardHasFourPieces :: Property
prop_initialBoardHasFourPieces = property $ do
  size <- forAll $ Gen.int (Range.linear 4 100)
  let pieces = concat (initBoard size)
  length (filter (/= Nothing) pieces) === 4

prop_currentPlayerAlternates :: Property
prop_currentPlayerAlternates = property $ do
  size <- forAll $ Gen.int (Range.linear 4 100)
  let initialBoard = initBoard size
  let initialGameState = GameState initialBoard Black []
  move1 <- forAll $ Gen.element (validMoves initialBoard Black)
  let stateAfterMove1 = applyMove initialGameState move1
  case stateAfterMove1 of
    Left _ -> success
    Right state1 -> do
      currentPlayer state1 === White
      move2 <- forAll $ Gen.element (validMoves (board state1) White)
      let stateAfterMove2 = applyMove state1 move2
      case stateAfterMove2 of
        Left _ -> success
        Right state2 -> do
          currentPlayer state2 === Black

prop_validMoveIsWithinBounds :: Property
prop_validMoveIsWithinBounds = property $ do
  size <- forAll $ Gen.int (Range.linear 4 100)
  let initialBoard = initBoard size
  move <- forAll $ Gen.element (validMoves initialBoard Black)
  let gameState = GameState initialBoard Black []
  let newGameState = applyMove gameState move
  case newGameState of
    Left _ -> success
    Right state -> do
      let (x, y) = move
      (x >= 0 && x < size && y >= 0 && y < size) === True
