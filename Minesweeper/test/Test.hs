import Test.Tasty

import qualified Test.TestGrid as TestGrid
import qualified Test.TestGameLogic as TestGameLogic
import qualified Test.TestConsole as TestConsole
import qualified Test.TestErrorHandling as TestErrorHandling

main :: IO ()
main = do
    defaultMain (testGroup "All Tests"
                    [ testGroup "Grid" [TestGrid.tests]
                    , testGroup "GameLogic" [TestGameLogic.tests]
                    , testGroup "Console" [TestConsole.tests]
                    , testGroup "ErrorHandling" [TestErrorHandling.tests]
                    ])
