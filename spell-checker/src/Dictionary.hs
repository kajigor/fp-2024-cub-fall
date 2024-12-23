module Dictionary (
    Dictionary,
    getDictionary,
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
import qualified Data.Vector.Unboxed as V
import Data.List (sortOn, foldl')
import Control.Parallel.Strategies (parList, rpar, using)
import qualified Data.HashMap.Strict as HM

-- Assuming Dictionary is something like this:
data Dictionary = Dictionary { getDict :: HM.HashMap T.Text Bool }

-- Function to extract the underlying HashMap from the Dictionary
getDictionary :: Dictionary -> HM.HashMap T.Text Bool
getDictionary = getDict

-- Load the dictionary from a file
loadDictionary :: FilePath -> IO Dictionary
loadDictionary path = do
    content <- TIO.readFile path
    let wordsList = map T.toLower (T.lines content) 
    return $ Dictionary (HM.fromList $ zip wordsList (repeat True)) 

addWordToDictionary :: Dictionary -> T.Text -> Dictionary
addWordToDictionary dict word =
    let dictMap = getDictionary dict
        updatedDictMap = HM.insert (T.toLower word) True dictMap
    in Dictionary updatedDictMap

-- Check if a word exists in the dictionary
checkWord :: Dictionary -> T.Text -> Bool
checkWord dict word = HM.member (T.toLower word) (getDictionary dict)

-- Suggest corrections using Damerau-Levenshtein distance
suggestCorrections :: HM.HashMap T.Text a -> T.Text -> [T.Text]
suggestCorrections dict word =
    let cleanedWord = cleanWord word
    in if HM.member cleanedWord dict
       then [] -- Word already exists in the dictionary
       else take 5 $ sortOn (editDistance cleanedWord) plausibleKeys
  where
    -- Filter plausible keys by length
    plausibleKeys = filter (\k -> abs (T.length k - T.length word) <= 2) $ HM.keys dict

-- Damerau-Levenshtein distance using vectors
editDistance :: T.Text -> T.Text -> Int
editDistance s1 s2 = V.last currRow
  where
    t1 = T.unpack s1
    t2 = T.unpack s2
    m = length t1
    n = length t2

    -- Initialize the first row
    initialRow = V.fromList [0 .. n]

    -- Compute the last row using fold
    currRow = foldl' updateRow initialRow [1 .. m]

    -- Update a single row
    updateRow :: V.Vector Int -> Int -> V.Vector Int
    updateRow prevRow i = V.generate (n + 1) $ \j ->
        if j == 0
        then i -- First column
        else let cost = if t1 !! (i - 1) == t2 !! (j - 1) then 0 else 1
                 deletion = prevRow V.! j + 1
                 insertion = prevRow V.! (j - 1) + 1
                 substitution = prevRow V.! (j - 1) + cost
                 transposition =
                     if i > 1 && j > 1 &&
                        t1 !! (i - 1) == t2 !! (j - 2) &&
                        t1 !! (i - 2) == t2 !! (j - 1)
                     then prevRow V.! (j - 2) + 1
                     else maxBound
             in minimum [deletion, insertion, substitution, transposition]

printSuggestions :: [T.Text] -> IO ()
printSuggestions suggestions =
    mapM_ (\s -> TIO.putStrLn $ T.pack "- " <> s) suggestions

-- Add a custom word to the dictionary
addCustomWord :: T.Text -> Dictionary -> Dictionary
addCustomWord word dict =
    let dictMap = getDictionary dict
        updatedDictMap = HM.insert word True dictMap
    in Dictionary updatedDictMap

serializeDictionary :: Dictionary -> T.Text
serializeDictionary dict =
    let dictMap = getDictionary dict in
    T.unlines [word | (word, isCorrect) <- HM.toList dictMap, isCorrect]

saveCustomWords :: T.Text -> Dictionary -> IO ()
saveCustomWords path dict = TIO.writeFile (T.unpack path) (serializeDictionary dict)

-- Function to clean words (removes non-alphabetic characters)
cleanWord :: T.Text -> T.Text
cleanWord = T.filter isAlpha . T.map toLower

-- Function to split the text into words and clean each word
splitWords :: T.Text -> [T.Text]
splitWords = map cleanWord . T.words