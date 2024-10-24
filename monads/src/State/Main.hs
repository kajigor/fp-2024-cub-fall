module State.Main where 

import qualified State.Renamer as Renamer 
import qualified State.ConservativeRenamer as ConservativeRenamer
import Util 
import Expr 
import Text.Printf 
import HW.Compiler


main :: IO () 
main = do 
  -- runRenamer 
  -- runConservativeRenamer
  print $ compile expr1

runConservativeRenamer :: IO ()
runConservativeRenamer = do 
    putStrLn (thickEnclose "Conservative Renamer examples")
    mapM_ (run ConservativeRenamer.runRename) exprs
 
runRenamer :: IO ()
runRenamer = do 
    putStrLn (thickEnclose "Renamer examples")
    mapM_ (run Renamer.runRename) exprs

run :: (Show t, Show a) => (t -> a) -> t -> IO ()
run renamer e = do
  print e
  putStrLn $ printf "Result: \n%s\n" (show $ renamer e)