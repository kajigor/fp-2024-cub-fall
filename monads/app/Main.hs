module Main (main) where

import Expr
import qualified Reader.Main as Reader 
import qualified Writer.Main as Writer 
import qualified State.Main as State 
import qualified HW.Main as SM 
import qualified Cont.Main as C 
import qualified FailCont.Main as Fail


main :: IO ()
main = do 
  Fail.main
  -- C.main 
  -- SM.main 
  -- State.main
  -- Reader.main
  -- Writer.main
