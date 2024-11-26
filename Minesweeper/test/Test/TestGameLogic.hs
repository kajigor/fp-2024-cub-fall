module Test.TestGameLogic where

import Test.Tasty
import Test.Tasty.HUnit
import GameLogic
import Grid

testRevealCell :: TestTree
testRevealCell = testCase "Reveal cell" $ do
    let rows = 3
        cols = 3
        mines = 1
        minePositions = [(1, 1)]
    grid <- initializeGrid rows cols mines (Just minePositions)
    let revealedGrid = revealCell grid (0, 0)
    let expectedGrid = [[Empty 1, Empty 1, Empty 1],
                        [Empty 1, Mine,    Empty 1],
                        [Empty 1, Empty 1, Empty 1]]
    assertEqual "Revealing a cell should reveal adjacent cells if no adjacent mines" expectedGrid revealedGrid

testIsWin :: TestTree
testIsWin = testCase "Check win condition" $ do
    let grid = [[Empty 1, Empty 1, Empty 1],
                [Empty 1, Mine,    Empty 1],
                [Empty 1, Empty 1, Empty 1]]
    assertBool "Should be a win if all non-mine cells are revealed" (isWin grid)

testIsLoss :: TestTree
testIsLoss = testCase "Check loss condition" $ do
    let grid = [[Empty 1, Empty 1, Empty 1],
                [Empty 1, Mine,    Empty 1],
                [Empty 1, Empty 1, Empty 1]]
    let revealedGrid = revealCell grid (1, 1)
    assertBool "Should be a loss if a mine is revealed" (isLoss revealedGrid)

testFlagCell :: TestTree
testFlagCell = testCase "Flag and unflag cell" $ do
    let rows = 3
        cols = 3
        mines = 1
        minePositions = [(1, 1)]
    grid <- initializeGrid rows cols mines (Just minePositions)
    let flaggedGrid = flagCell grid (0, 0)
    let unflaggedGrid = flagCell flaggedGrid (0, 0)
    assertEqual "Flagging a cell should mark it as flagged" (Just Flagged) (getCell flaggedGrid (0, 0))
    assertEqual "Unflagging a cell should revert it to hidden" (Just Hidden) (getCell unflaggedGrid (0, 0))

tests :: TestTree
tests = testGroup "GameLogic Tests"
    [ testRevealCell
    , testIsWin
    , testIsLoss
    , testFlagCell
    ]
