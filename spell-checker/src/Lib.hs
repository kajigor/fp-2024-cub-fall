module Lib where

import qualified Data.Map as Map
import Data.Char (isAlpha, toLower)
import System.IO (withFile, IOMode(ReadMode), hGetLine, hIsEOF)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Array
import qualified Data.PQueue.Min as PQ

-- Trie data structure
data Trie = Trie
  { children :: Map.Map Char Trie
  , isEndOfWord :: Bool
  } deriving (Show)

-- Initialize an empty Trie
emptyTrie :: Trie
emptyTrie = Trie Map.empty False

-- Insert a word into the Trie
insert :: String -> Trie -> Trie
insert [] trie = trie { isEndOfWord = True }
insert (x:xs) trie =
  let child = Map.findWithDefault emptyTrie x (children trie)
      newChild = insert xs child
  in trie { children = Map.insert x newChild (children trie) }

-- Search for a word in the Trie
search :: String -> Trie -> Bool
search [] trie = isEndOfWord trie
search (x:xs) trie =
  case Map.lookup x (children trie) of
    Just child -> search xs child
    Nothing -> False

-- Collect all words in the Trie
collectWords :: Trie -> String -> [String]
collectWords trie prefix =
  let suffixes = concatMap (\(c, child) -> collectWords child (prefix ++ [c]))
                  (Map.assocs $ children trie)
  in if isEndOfWord trie then prefix : suffixes else suffixes

-- Optimized Levenshtein distance with dynamic programming
levenshtein :: String -> String -> Int
levenshtein s1 s2 = table ! (m, n)
  where
    m = length s1
    n = length s2
    s1' = '#' : s1 
    s2' = '#' : s2

    table = array ((0, 0), (m, n))
      [((i, j), value i j) | i <- [0 .. m], j <- [0 .. n]]

    value 0 j = j 
    value i 0 = i 
    value i j
      | s1' !! i == s2' !! j = table ! (i - 1, j - 1) 
      | otherwise =
        1 + minimum
          [ table ! (i - 1, j),    
            table ! (i, j - 1),    
            table ! (i - 1, j - 1) 
          ]

-- Optimized function to suggest corrections using Trie traversal
suggestCorrections :: Trie -> String -> Int -> [String]
suggestCorrections trie word tolerance =
  let
    searchTrie :: Trie -> String -> Int -> String -> PQ.MinQueue (Int, String) -> PQ.MinQueue (Int, String)
    searchTrie trie remainingWord currentDistance prefix queue
      | currentDistance > tolerance = queue
      | otherwise =
        let updatedQueue =
              if null remainingWord && isEndOfWord trie
              then PQ.insert (currentDistance, prefix) queue
              else queue
            nextChars = Map.assocs (children trie)
            nextQueue = foldl (\q (c, child) ->
                              let newRemainingWord = drop 1 remainingWord
                                  nextDistance = case remainingWord of
                                    [] -> currentDistance + 1  
                                    (r:_) -> if c == r then currentDistance else currentDistance + 1
                              in if nextDistance <= tolerance
                                 then searchTrie child newRemainingWord nextDistance (prefix ++ [c]) q
                                 else q 
                           ) updatedQueue nextChars
      in nextQueue


    initialQueue = PQ.empty
    resultQueue = searchTrie trie word 0 "" initialQueue
  in take 5 [w | (_, w) <- PQ.toAscList resultQueue] 


-- Check if a token is a valid word
isWord :: String -> Bool
isWord = all isAlpha

-- Process a single word, suggesting corrections if necessary
processWord :: Trie -> String -> IO String
processWord trie word
    | isWord word =
        if search (map toLower word) trie 
        then return word
        else suggestCorrection trie word
    | otherwise = return word

--- Suggest corrections interactively
suggestCorrection :: Trie -> String -> IO String
suggestCorrection trie word = do
    let tolerance = 2
    let suggestions = take 5 $ suggestCorrections trie (map toLower word) tolerance
    if null suggestions
        then do
            putStrLn $ "The word \"" ++ word ++ "\" is misspelled. No suggestions available."
            putStrLn "Enter a replacement word, or press Enter to skip:"
            choice <- getLine
            if null choice
                then return word  
                else return choice 
        else do
            putStrLn $ "The word \"" ++ word ++ "\" is misspelled. Suggestions:"
            mapM_ (\(i, s) -> putStrLn (show i ++ ". " ++ s)) (zip [1..] suggestions)
            putStrLn "Enter the number of the correct word, 0 to skip, or type a replacement:"
            choice <- getLine
            case reads choice :: [(Int, String)] of
                [(0, _)] -> return word  
                [(n, _)] | n > 0 && n <= length suggestions ->
                    return (suggestions !! (n - 1)) 
                _ -> return choice 

-- Spell-check a paragraph interactively
spellCheckInteractive :: Trie -> String -> IO String
spellCheckInteractive trie paragraph = do
    wordsInParagraph <- mapM (processWord trie) (words paragraph)
    return $ unwords wordsInParagraph

-- Check the text interactively
checkText :: Trie -> IO ()
checkText trie = do
    putStrLn "Enter a paragraph:"
    paragraph <- getLine
    correctedParagraph <- spellCheckInteractive trie paragraph
    putStrLn "Corrected paragraph:"
    putStrLn correctedParagraph

-- Add a word to the Trie and update the dictionary file
addWord :: Trie -> String -> IO Trie
addWord trie word = do
    let lowerWord = map toLower word
    let updatedTrie = insert lowerWord trie
    appendFile "dictionary.txt" (lowerWord ++ "\n")
    return updatedTrie

-- Load words from the dictionary file into the Trie
loadDictionary :: Trie -> IO Trie
loadDictionary trie = withFile "dictionary.txt" ReadMode $ \handle -> do
    let loop accTrie = do
            eof <- hIsEOF handle
            if eof
                then return accTrie
                else do
                    line <- hGetLine handle
                    let updatedTrie = insert (map toLower line) accTrie
                    loop updatedTrie
    loop trie
