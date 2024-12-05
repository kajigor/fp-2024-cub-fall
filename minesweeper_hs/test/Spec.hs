import Test.HUnit
import Test.QuickCheck
import qualified Data.Set as Set
import Minesweeper

testInitGame :: Test
testInitGame = TestCase $ do
    game <- initGame 5 5 3
    assertEqual "Game should have correct dimensions" (height game, width game) (5, 5)
    assertBool "Game should have the correct number of mines" (Set.size (mines game) == 3)
    assertBool "No cells should be revealed initially" (Set.null (revealed game))
    assertBool "No cells should be flagged initially" (Set.null (flagged game))

testRevealSafeCell :: Test
testRevealSafeCell = TestCase $ do
    let game = Minesweeper 3 3 (Set.fromList [(1, 1)]) Set.empty Set.empty
    let updatedGame = makeMove game (0, 0)
    assertBool "Safe cell should be revealed" (Set.member (0, 0) (revealed updatedGame))
    let n = nearbyMines updatedGame (0, 0)
    assertEqual "Nearby mines count should be correct" n 1

testHitMine :: Test
testHitMine = TestCase $ do
    let game = Minesweeper 3 3 (Set.fromList [(1, 1)]) Set.empty Set.empty
    assertBool "Cell (1,1) should be a mine" (isMine game (1, 1))

testWinCondition :: Test
testWinCondition = TestCase $ do
    let game = Minesweeper 2 2 (Set.fromList [(0, 0)]) (Set.fromList [(1, 0), (0, 1), (1, 1)]) Set.empty
    assertBool "Player should win when all safe cells are revealed" (won game)

testInvalidMoves :: Test
testInvalidMoves = TestCase $ do
    let game = Minesweeper 3 3 (Set.fromList [(1, 1)]) Set.empty Set.empty
    let updatedGame = makeMove game (0, 0)
    assertBool "Revealing an already revealed cell should do nothing" (makeMove updatedGame (0, 0) == updatedGame)
    
testFlagging :: Test
testFlagging = TestCase $ do
    let game = Minesweeper 3 3 Set.empty Set.empty Set.empty
    let flaggedGame = game {flagged = Set.insert (0, 0) (flagged game)}
    assertBool "Cell (0,0) should be flagged" (Set.member (0, 0) (flagged flaggedGame))
    let unflaggedGame = flaggedGame {flagged = Set.delete (0, 0) (flagged flaggedGame)}
    assertBool "Cell (0,0) should be unflagged" (not (Set.member (0, 0) (flagged unflaggedGame)))

propMinesWithinBounds :: Int -> Int -> Int -> Property
propMinesWithinBounds h w m =
    h > 0 && w > 0 && m > 0 ==> ioProperty $ do
        game <- initGame h w m
        return (all (\(x, y) -> x >= 0 && x < h && y >= 0 && y < w) (Set.toList (mines game)))

tests :: Test
tests =
    TestList
        [ TestLabel "Game Initialization" testInitGame,
          TestLabel "Reveal Safe Cell" testRevealSafeCell,
          TestLabel "Hit Mine" testHitMine,
          TestLabel "Win Condition" testWinCondition,
          TestLabel "Invalid Moves" testInvalidMoves,
          TestLabel "Flagging and Unflagging" testFlagging
        ]

main :: IO ()
main = do
    _ <- runTestTT tests
    quickCheck propMinesWithinBounds
