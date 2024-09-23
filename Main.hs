{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Use product" #-}
{-# HLINT ignore "Use sum" #-}
module Main where

import Control.Monad (unless)
import qualified Data.List as L
import Text.Printf (printf)

multByIndex :: [Int] -> [Int]
multByIndex xs = [ elem * index | (elem, index) <- zip xs [0..] ]

powerByIndex :: [Int] -> [Int]
powerByIndex xs = [ elem ^ index | (elem, index) <- zip xs [0..] ]

productOfDifferenceHelper :: [Int] -> Int
productOfDifferenceHelper [_] = 1
productOfDifferenceHelper (first:tail@(second:_))= (first - second) *  productOfDifferenceHelper tail

productOfDifference :: [Int] -> Int
productOfDifference [] = error "The list is too short"
productOfDifference [_] = error "The list is too short"
productOfDifference xs = productOfDifferenceHelper xs

isSorted :: [Int] -> Bool 
isSorted [] = True
isSorted [_] = True
isSorted (first:tail@(second:xs)) = first <= second && isSorted tail 

countElement :: Int -> [Int] -> Int 
countElement target [] = 0
countElement target (x:xs) = countElement target xs + if x == target then 1 else 0

dotProduct :: [Int] -> [Int] -> Int 
dotProduct lhs rhs = sum [ first * second | (first, second) <- zip lhs rhs ]

applyAll :: [a -> b] -> a -> [b]
applyAll functions arg = [f arg | f <- functions]

interleave :: [a] -> [a] -> [a]
interleave _ [] = []
interleave [] _ = [] 
interleave (x:xs) (y:ys) = x:y:interleave xs ys

main = do
  runTests
  putStrLn "Done"

runTests = do
    runMultByIndex
    runPowerByIndex
    runProductOfDifference
    runIsSorted 
    runCountElement 
    runDotProduct 
    runApplyAll 
    runInterleave
  where
    runMultByIndex = do 
        mapM_ (uncurry $ test1 "multByIndex" multByIndex) testCases 
        test1 "multByIndex, infinite input" (take 10 . multByIndex) [1 ..] [0, 2, 6, 12, 20, 30, 42, 56, 72, 90]
      where 
        testCases = 
          [ ([], [])
          , ([1..10], [0,2,6,12,20,30,42,56,72,90])
          , ([-1, -2, -3], [0, -2, -6])
          ]

    runPowerByIndex = do 
        mapM_ (uncurry $ test1 "powerByIndex" powerByIndex) testCases 
        test1 "powerByIndex, infinite input" (take 10 . powerByIndex) [1 ..] [1,2^1,3^2,4^3,5^4,6^5,7^6,8^7,9^8,10^9]
      where 
        testCases = 
          [ ([], [])
          , ([1..10], [1,2^1,3^2,4^3,5^4,6^5,7^6,8^7,9^8,10^9])
          , ([-1, -2, -3], [1, (-2)^1, (-3)^2])
          ]
    
    runProductOfDifference = do 
        mapM_ (uncurry $ test1 "productOfDifference" productOfDifference) testCases
      where 
        testCases = 
          [ ([10, 10], 0)
          , ([42, 13], 42 - 13)
          , ([(-5), (-3) .. 5], -(2^5))
          , ([10, 9 .. 0], 1)
          , ([100000, 99999, 99998], 1)
          , ([2033, 2925, 6087, 272, 3643], (2033 - 2925) * (2925 - 6087) * (6087 - 272) * (272 - 3643))
          ]

    runIsSorted = do 
        mapM_ (uncurry $ test1 "isSorted" isSorted) testCases 
      where 
        testCases = 
          [ ([], True)
          , ([1..10], True)
          , ([10,8 .. 1], False)
          , ([1,2,3,4,5,-1], False)
          , (replicate 13 0, True)
          , ([13, 542, 1, 3,4,5,5], False)
          ]

    uncurry3 f (x,y,z) = f x y z 

    runCountElement = do 
        mapM_ (uncurry3 $ test2 "countElement" countElement) testCases
      where 
        testCases = 
          [ (12, [], 0)
          , (12, replicate 12 12, 12 )
          , (13, replicate 12 12, 0)
          , (42, [0..100], 1)
          , (42, [ if even x then 42 else x | x <- [0..1000]], 501)
          , (42, [1, 42, 2, 42, 42, 42, 4, 42], 5)
          ]

    runDotProduct = do 
        mapM_ (uncurry3 $ test2 "dotProduct" dotProduct) testCases 
      where 
        testCases = 
          [ ([], [], 0)
          , ([2], [3], 6)
          , ([1,2,3], [4,5,6], 32)
          , ([-1, -2, -3], [4,5,6], -32)
          , (replicate 10 0, [0..9], 0)
          , ([1, 0, 3], [0, 2, 0], 0)
          , ([2,1], [3], 6)
          , ([2], [3,1], 6)
          ]

    runApplyAll = do 
        mapM_ (uncurry3 $ test2 "applyAll" applyAll) testCases 
        mapM_ (uncurry3 $ test2 "applyAll" applyAll) testCasesList 
      where 
        test2 functionName f inp1 inp2 exp = 
          let act = f inp1 inp2 in 
          if act == exp 
          then return () 
          else putStrLn "applyAll test Failed"

        testCases = 
          [ ([] :: [Int -> Int], 42, [])
          , (replicate 13 id, 42, replicate 13 42)
          , ([(*2), (+3), (^2)], 42, [42*2, 42+3, 42^2])
          , (replicate 5 (const 42), 13, replicate 5 42)
          ]
        testCasesList = 
          [ ([length, const 0, length . take 3], "Hello", [5, 0, 3])
          ]

    runInterleave = do 
        mapM_ (uncurry3 $ test2 "interleave" interleave) testCases 
      where 
        testCases = 
          [ ([], [], [])
          , ([1], [2], [1,2])
          , ([1..10], [10,9..1], [1,10,2,9,3,8,4,7,5,6,6,5,7,4,8,3,9,2,10,1])
          ]
   
    describeFailure :: (Show a, Show b) => String -> a -> b -> b -> IO ()
    describeFailure functionName input exp actual =
      putStrLn $
        printf
          "Test for a function %s has failed:\n  Input: %s\n  Expected: %s\n  But got: %s\n"
          functionName
          (show input)
          (show exp)
          (show actual)
    
    test1 :: (Show a, Show b, Eq b) => String -> (a -> b) -> a -> b -> IO () 
    test1 functionName f inp exp = 
      let act = f inp in 
      if act == exp 
      then return () 
      else describeFailure functionName inp exp act

    test2 :: (Show a, Show b, Show c, Eq c) => String -> (a -> b -> c) -> a -> b -> c -> IO ()
    test2 functionName f inp1 inp2 exp = 
      let act = f inp1 inp2 in 
      if act == exp 
      then return () 
      else describeFailure functionName (inp1, inp2) exp act