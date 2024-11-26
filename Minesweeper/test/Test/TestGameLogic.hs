module Test.TestGameLogic where

import Test.Tasty
import Test.Tasty.HUnit
import GameLogic
import Grid

testRevealCell :: TestTree
testRevealCell = testCase "Reveal cell on 5x5 grid" $ do
    let rows = 5
        cols = 5
        mines = 1
        minePositions = [(2, 2)] -- Place a mine at the center of the grid
    grid <- initializeGrid rows cols mines (Just minePositions)

    -- Reveal a corner cell (0, 0) and verify propagation
    let revealedGrid = revealCell grid (0, 0)

    -- Expected grid after revealing (0, 0):
    -- Adjacent cells with no mines should propagate, stopping at cells with numbers
    let expectedRevealed = 
            [ [Empty 0, Empty 0, Empty 0, Empty 0, Empty 0],
              [Empty 0, Empty 1, Empty 1, Empty 1, Empty 0],
              [Empty 0, Empty 1, Mine,    Empty 1, Empty 0],
              [Empty 0, Empty 1, Empty 1, Empty 1, Empty 0],
              [Empty 0, Empty 0, Empty 0, Empty 0, Empty 0] ]

    -- Compare expected and actual revealed grids
    assertEqual "Revealing cell (0,0) should reveal the appropriate adjacent cells" expectedRevealed revealedGrid

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
