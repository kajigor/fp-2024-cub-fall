module Main (main) where

import Interpreter (runInterpreter)
import qualified Data.Map.Strict as M
import qualified Control.Monad.Trans.State.Strict as T

main :: IO ()
main = T.evalStateT runInterpreter $ M.fromList [("ans", 0), ("mem", 0)]
