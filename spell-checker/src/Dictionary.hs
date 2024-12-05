module Dictionary (
    Dictionary,
    loadDictionary,
    saveCustomWords,
    checkWord,
    cleanWord,
    suggestCorrections,
    addCustomWord,
    printSuggestions,
    splitWords
) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Array

type Dictionary = Map.Map T.Text Bool

-- Load the dictionary from a file
loadDictionary :: FilePath -> IO Dictionary
loadDictionary path = do
    content <- TIO.readFile path
    let wordsList = map T.toLower (T.lines content)
    return $ Map.fromList $ zip wordsList (repeat True)

-- Add a word to the dictionary
addWordToDictionary :: Dictionary -> T.Text -> Dictionary
addWordToDictionary dict word = Map.insert (T.toLower word) True dict

-- Check if a word exists in the dictionary
checkWord :: Dictionary -> T.Text -> Bool
checkWord dict word = Map.member (T.toLower word) dict

-- Suggest corrections using Damerau-Levenshtein distance
suggestCorrections :: Dictionary -> T.Text -> [T.Text]
suggestCorrections dict word =
    let cleanedWord = cleanWord word
    in if Map.member cleanedWord dict
       then [] 
       else take 5 $ sortOn (editDistance cleanedWord) $ Map.keys dict

-- Damerau-Levenshtein distance
editDistance :: T.Text -> T.Text -> Int
editDistance s1 s2 = d ! (m, n)
  where
    t1 = T.unpack s1
    t2 = T.unpack s2
    m = length t1
    n = length t2

    -- Array for storing intermediate distances
    d = array ((0, 0), (m, n))
        [((i, j), distance i j) | i <- [0..m], j <- [0..n]]

    -- Compute the distance at (i, j)
    distance 0 0 = 0
    distance i 0 = i
    distance 0 j = j
    distance i j
        | t1 !! (i - 1) == t2 !! (j - 1) = d ! (i - 1, j - 1)
        | otherwise = minimum
            [ d ! (i - 1, j) + 1    -- Deletion
            , d ! (i, j - 1) + 1    -- Insertion
            , d ! (i - 1, j - 1) + 1 -- Substitution
            , if i > 1 && j > 1 && t1 !! (i - 1) == t2 !! (j - 2)
                   && t1 !! (i - 2) == t2 !! (j - 1)
              then d ! (i - 2, j - 2) + 1 -- Transposition
              else maxBound
            ]

printSuggestions :: [T.Text] -> IO ()
printSuggestions suggestions =
    mapM_ (\s -> TIO.putStrLn $ T.pack "- " <> s) suggestions

addCustomWord :: T.Text -> Dictionary -> Dictionary
addCustomWord word dict = Map.insert word True dict

saveCustomWords :: T.Text -> Dictionary -> IO ()
saveCustomWords path dict = TIO.writeFile (T.unpack path) (serializeDictionary dict)

serializeDictionary :: Dictionary -> T.Text
serializeDictionary dict =
    T.unlines [word | (word, isCorrect) <- Map.toList dict, isCorrect]

-- Function to clean words (removes non-alphabetic characters)
cleanWord :: T.Text -> T.Text
cleanWord = T.filter isAlpha . T.map toLower

-- Function to split the text into words and clean each word
splitWords :: T.Text -> [T.Text]
splitWords = map cleanWord . T.words