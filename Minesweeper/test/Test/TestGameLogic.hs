module Test.TestGameLogic where

import Test.Tasty
import Test.Tasty.HUnit
import Grid
import GameLogic

-- Test Suite for Game Logic
tests :: TestTree
tests = testGroup "Game Logic Tests"
  [ testCase "Reveal an empty cell" $ do
      let grid = [[(Hidden, Empty 0), (Hidden, Empty 1), (Hidden, Mine)],
                  [(Hidden, Mine), (Hidden, Empty 2), (Hidden, Empty 1)],
                  [(Hidden, Empty 1), (Hidden, Empty 1), (Hidden, Empty 0)]]
          expected = [[(Revealed, Empty 0), (Revealed, Empty 1), (Hidden, Mine)],
                      [(Revealed, Mine), (Revealed, Empty 2), (Hidden, Empty 1)],
                      [(Hidden, Empty 1), (Hidden, Empty 1), (Hidden, Empty 0)]]
      revealCell grid (0, 0) @?= expected

  , testCase "Reveal a mine" $ do
      let grid = [[(Hidden, Mine), (Hidden, Empty 1)],
                  [(Hidden, Empty 1), (Hidden, Empty 0)]]
          expected = [[(Revealed, Mine), (Hidden, Empty 1)],
                      [(Hidden, Empty 1), (Hidden, Empty 0)]]
      revealCell grid (0, 0) @?= expected

  , testCase "Flag a cell" $ do
      let grid = [[(Hidden, Empty 0), (Hidden, Mine)],
                  [(Hidden, Empty 1), (Hidden, Empty 0)]]
          expected = [[(Flagged, Empty 0), (Hidden, Mine)],
                      [(Hidden, Empty 1), (Hidden, Empty 0)]]
      flagCell grid (0, 0) @?= expected

  , testCase "Unflag a flagged cell" $ do
      let grid = [[(Flagged, Empty 0), (Hidden, Mine)],
                  [(Hidden, Empty 1), (Hidden, Empty 0)]]
          expected = [[(Hidden, Empty 0), (Hidden, Mine)],
                      [(Hidden, Empty 1), (Hidden, Empty 0)]]
      flagCell grid (0, 0) @?= expected

  , testCase "Win condition: all non-mine cells revealed" $ do
      let grid = [[(Revealed, Empty 1), (Hidden, Mine)],
                  [(Revealed, Empty 1), (Revealed, Empty 0)]]
      isWin grid @?= True

  , testCase "Lose condition: a mine is revealed" $ do
      let grid = [[(Revealed, Mine), (Hidden, Empty 1)],
                  [(Hidden, Empty 1), (Hidden, Empty 0)]]
      isLoss grid @?= True

  , testCase "Count mines in a grid" $ do
      let grid = [[(Hidden, Mine), (Hidden, Empty 1)],
                  [(Hidden, Mine), (Hidden, Empty 0)]]
      countMines grid @?= 2
  ]

-- Utility to count the number of mines in the grid
countMines :: Grid -> Int
countMines = sum . map (length . filter (\(_, cell) -> cell == Mine))
