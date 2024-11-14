module Writer.Main where 

import Writer.Transaction 
import Writer.MyWriter
import Data.List (intercalate)
import Text.Printf (printf)
import Util (thickEnclose)

performTransactions :: Int -> Transaction Int
performTransactions balance = do
  balance1 <- deposit balance 100
  balance2 <- withdraw balance1 150
  balance3 <- withdraw balance2 500
  balance4 <- withdraw balance3 150
  checkBalance balance4

showTransactions (MyWriter (w, a)) =
  printf "%s\nFinal balance: %d" (intercalate "\n" $ map show w) a

main = do   
  putStrLn $ thickEnclose "Transaction examples"
  putStrLn $ showTransactions $ performTransactions 200