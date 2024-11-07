module Main (main) where

import Expr
import qualified FailCont.Main as C


main :: IO ()
main = do
  C.main
  -- SM.main
  -- State.main
  -- Reader.main
  -- Writer.main
