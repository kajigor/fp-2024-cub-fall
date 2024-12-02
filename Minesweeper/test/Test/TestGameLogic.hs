{-# LANGUAGE ScopedTypeVariables #-}

module Test.TestGameLogic where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import GameLogic
import Grid

-- Unit Tests
unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testCase "Revealing a cell with no adjacent mines" $ do
      let grid = [[Hidden, Hidden, Hidden]
                 , [Hidden, Hidden, Hidden]
                 , [Hidden, Hidden, Mine]]
          expected = [[Empty 0, Empty 0, Empty 0]
                     , [Empty 0, Empty 1, Empty 1]
                     , [Empty 0, Empty 1, Mine]]
      revealCell grid (0, 0) @?= expected

  , testCase "Revealing a cell with adjacent mines" $ do
      let grid = [[Hidden, Hidden]
                 , [Mine, Hidden]]
          expected = [[Hidden, Empty 1]
                     , [Mine, Hidden]]
      revealCell grid (0, 1) @?= expected

  , testCase "Winning condition with all non-mine cells revealed" $ do
      let grid = [[Empty 0, Mine], [Empty 0, Empty 0]]
      isWin grid @?= True

  , testCase "Losing condition when a mine is revealed" $ do
      let grid = [[Mine, Hidden], [Hidden, Hidden]]
      let revealed = revealCell grid (0, 0)
      isLoss revealed @?= True
  ]

-- Property-based Tests
propertyTests :: TestTree
propertyTests = testGroup "Property Tests"
  [ testProperty "Revealing cells does not change the number of mines" $ property $ do
      let genGrid = generateGrid 5 5 3  -- 5x5 grid with 3 mines
      forAll genGrid >>= \grid -> do
          let initialMineCount = countMines grid
          let grid' = revealCell grid (0, 0)  -- arbitrary position
          countMines grid' === initialMineCount

  , testProperty "Winning condition implies all non-mine cells are revealed" $ property $ do
      let genGrid = generateGrid 5 5 3
      forAll genGrid >>= \grid -> do
          let revealedGrid = revealAllNonMines grid
          isWin revealedGrid === True
  ]

-- Generate random grids
generateGrid :: Int -> Int -> Int -> Gen Grid
generateGrid rows cols mines = do
  positions <- Gen.list (Range.singleton mines) randomPos
  return (addMines (emptyGrid rows cols) positions)
  where
    randomPos = (,) <$> Gen.int (Range.linear 0 (rows - 1)) <*> Gen.int (Range.linear 0 (cols - 1))
    emptyGrid r c = replicate r (replicate c Hidden)

-- Count the number of mines in a grid
countMines :: Grid -> Int
countMines = sum . map (length . filter (== Mine))

-- Reveal all non-mine cells (used for testing win condition)
revealAllNonMines :: Grid -> Grid
revealAllNonMines grid =
  [ [ if cell == Mine then cell else Empty 0 | cell <- row ]
    | row <- grid
  ]

tests :: TestTree
tests = testGroup "Game Logic Tests" [unitTests, propertyTests]
