module Util where

import           Text.Printf (printf)

enclose :: String -> String -> String -> String
enclose l r s =
  printf "\n%s\n%s\n%s\n" l s r

thickEnclose :: String -> String
thickEnclose =
  let thickLine = replicate 30 '=' in
  enclose thickLine thickLine

thinEnclose :: String -> String
thinEnclose =
  let thinLine = replicate 30 '-' in
  enclose thinLine thinLine

if' :: Bool -> a -> a -> a
if' True a _  = a
if' False _ b = b

-- Safe alternative to length
lengthLessThan :: [a] -> Int -> Bool
lengthLessThan _ 0      = False
lengthLessThan [] _     = True
lengthLessThan (_:xs) n = lengthLessThan xs (n-1)
