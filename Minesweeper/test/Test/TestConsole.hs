module Test.TestConsole where

import Test.Tasty
import Test.Tasty.HUnit
import System.IO.Silently (capture_)
import Grid
import GameLogic
import ConsoleUI

-- Test Suite for Console Logic
tests :: TestTree
tests = testGroup "Console Tests"
  [  testCase "Flag a cell" $ do
      let emptyGrid = replicate 5 (replicate 5 (Hidden, Empty 0))
      let flaggedGrid = flagCell emptyGrid (2, 2) -- Flag the cell at (2, 2)
      case getCell flaggedGrid (2, 2) of
        Just (Flagged, _) -> success
        _ -> assertFailure "Cell was not flagged correctly"

  , testCase "Unflag a cell" $ do
      let emptyGrid = replicate 5 (replicate 5 (Hidden, Empty 0))
      let flaggedGrid = flagCell emptyGrid (2, 2) -- Flag the cell at (2, 2)
      let unflaggedGrid = flagCell flaggedGrid (2, 2) -- Unflag the cell
      case getCell unflaggedGrid (2, 2) of
        Just (Hidden, _) -> success
        _ -> assertFailure "Cell was not unflagged correctly"

  , testCase "Reveal a cell" $ do
      let emptyGrid = replicate 5 (replicate 5 (Hidden, Empty 0))
      let revealedGrid = revealCell emptyGrid (1, 1) -- Reveal the cell at (1, 1)
      case getCell revealedGrid (1, 1) of
        Just (Revealed, Empty 0) -> success
        _ -> assertFailure "Cell was not revealed correctly"

  , testCase "Reveal a mine" $ do
      let mineGrid = [[(Hidden, Mine), (Hidden, Empty 1)], [(Hidden, Empty 0), (Hidden, Empty 0)]]
      let revealedGrid = revealCell mineGrid (0, 0) -- Reveal the cell with a mine
      case getCell revealedGrid (0, 0) of
        Just (Revealed, Mine) -> success
        _ -> assertFailure "Mine was not revealed correctly"
  ]

success :: IO ()
success = return ()
