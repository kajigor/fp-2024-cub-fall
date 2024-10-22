module Main (main) where

import Expr
import qualified Reader.Main as Reader 
import qualified Writer.Main as Writer 
import qualified State.Main as State 
import qualified HW.Main as SM 


main :: IO ()
main = do 
  SM.main 
  -- State.main
  -- Reader.main
  -- Writer.main
