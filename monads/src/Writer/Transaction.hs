module Writer.Transaction where 

import Writer.MyWriter ( MyWriter(MyWriter), tell )
import Text.Printf (printf)
import Data.List (intercalate)

data Log 
  = Error String 
  | Warn String 
  | Info String 
  deriving (Show)

logErr :: String -> MyWriter [Log] () 
logErr = tell . (:[]) . Error 

logWarn :: String -> MyWriter [Log] ()
logWarn = tell . (:[]) . Warn 

logInfo :: String -> MyWriter [Log] ()
logInfo = tell . (: []) . Info

type Transaction a = MyWriter [Log] a 

deposit :: Int -> Int -> Transaction Int 
deposit balance amount = undefined 

withdraw :: Int -> Int -> Transaction Int 
withdraw balance amount = undefined

checkBalance :: Int -> Transaction Int 
checkBalance balance = undefined