module Main (main) where
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Game
import Logic
import Rendering
import Data.Array
import Control.Exception (assert)

initialGame :: Game
initialGame = Game 
    { n = n1
    , gameBoard = initialBoard
    , player = Player1
    , state = Running
    , screenWidth = n2
    , screenHeight = n3
    , cellWidth = fromIntegral n2 / fromIntegral n1
    , cellHeight = fromIntegral n3 / fromIntegral n1
    }
  where
    n1 = 8
    n1Half = n1 `div` 2
    n2 = 800
    n3 = 800
    indexRange = ((0, 0), (n1 - 1, n1 - 1))
    initialBoard = (array indexRange $ zip (range indexRange) (repeat Nothing)) // 
        [ ((n1Half - 1, n1Half - 1), Just Player1)
        , ((n1Half, n1Half), Just Player1)
        , ((n1Half - 1, n1Half), Just Player2)
        , ((n1Half, n1Half - 1), Just Player2)
        ]

testInitialBoard :: TestTree
testInitialBoard = testCase "Initial board state" $ do
    let board = gameBoard initialGame
    assertEqual "Initial cell (3,3)" (Just Player1) (board ! (3, 3))
    assertEqual "Initial cell (4,4)" (Just Player1) (board ! (4, 4))
    assertEqual "Initial cell (3,4)" (Just Player2) (board ! (3, 4))
    assertEqual "Initial cell (4,3)" (Just Player2) (board ! (4, 3))

testSafeAccess :: TestTree
testSafeAccess = testCase "Safe access on board" $ do
    let board = gameBoard initialGame
    assertEqual "Out-of-bounds negative" Nothing (safeAccess board 8 (-1, -1))
    assertEqual "Out-of-bounds high" Nothing (safeAccess board 8 (8, 8))
    assertEqual "Valid cell access" Nothing (safeAccess board 8 (0, 0))

testPlayerTurn :: TestTree
testPlayerTurn = testCase "Player turn alternation" $ do
    let game1 = initialGame
    let game2 = playerTurn game1 (3, 5)
    assertEqual "After one turn" Player2 (player game2)
    let game3 = playerTurn game2 (0, 0)
    assertEqual "Invalid second turn" Player2 (player game3)

testIsDirectionValid :: TestTree
testIsDirectionValid = testCase "Valid move directions" $ do
    let game = playerTurn (playerTurn initialGame (3, 5)) (2, 3)
    assertBool "Invalid direction for Player1" (not $ isDirectionValid game (5, 4) (1, 0))
    assertBool "Invalid direction for Player1" (not $ isDirectionValid game (3, 2) (1, 1))
    assertBool "Valid direction for Player1" (isDirectionValid game (5, 2) (-1, 1))

testEndGame :: TestTree
testEndGame = testCase "Game ends correctly" $ do
    let game = initialGame { state = GameOver (Just Player1) }
    assertEqual "Game over state" (GameOver (Just Player1)) (state game)

prop_BoardSize :: TestTree
prop_BoardSize = testProperty "Board size remains consistent" $
    \move -> let game = initialGame in
             let updatedGame = playerTurn game move in
             bounds (gameBoard updatedGame) == bounds (gameBoard game)

testRendering :: TestTree
testRendering = testCase "Rendering produces valid picture" $ do
    let picture = gameAsPicture initialGame
    assertBool "Rendering produces a picture" (picture /= boardRunningPicture initialGame)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Reversi Game Tests"
  [ testGroup "Unit Tests"
      [ testInitialBoard
      , testSafeAccess
      , testPlayerTurn
      , testIsDirectionValid
      , testEndGame
      , testRendering
      ]
  , testGroup "Property Tests"
      [ prop_BoardSize
      ]
  ]