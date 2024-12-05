module Dictionary (Dictionary (..), fromFile, fromList, fromJsonString, addWord, removeWord, getWordCount, hasWord, toFile) where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Prelude hiding (words)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as B
import qualified Data.String as S
import System.Exit (die)

data Dictionary = Dictionary
  { getDictionary :: HashMap.HashMap String Int,
    getKeys :: HashSet.HashSet String,
    getTotal :: Int
  }
  deriving (Show, Eq)

fromList :: [String] -> Dictionary
fromList allWords =
  Dictionary
    { getDictionary = HashMap.fromListWith (+) [(word, 1) | word <- allWords],
      getKeys = HashSet.fromList allWords,
      getTotal = length allWords
    }

fromByteString :: B.ByteString -> Maybe Dictionary
fromByteString content =
  let map = decode content :: Maybe (HashMap.HashMap String Int)
    in case map of
      Just words -> return $ Dictionary words (HashMap.keysSet words) (sum (HashMap.elems words))
      Nothing -> Nothing

fromJsonString :: String -> Maybe Dictionary
fromJsonString content = fromByteString (S.fromString content)

fromFile :: String -> IO Dictionary
fromFile filename = do
  content <- B.readFile filename
  case fromByteString content of
    Just dict -> return dict
    Nothing -> die "Invalid dictionary file"

addWord :: Dictionary -> String -> Dictionary
addWord (Dictionary words keys total) word =
  Dictionary (HashMap.insertWith (+) word 1 words) (HashSet.insert word keys) (total + 1)

removeWord :: Dictionary -> String -> Dictionary
removeWord (Dictionary words keys total) word =
  case HashMap.lookup word words of
    Just count -> Dictionary (HashMap.delete word words) (HashSet.delete word keys) (total - count)
    Nothing -> Dictionary words keys total

getWordCount :: Dictionary -> String -> Int
getWordCount (Dictionary words _ _) word = HashMap.lookupDefault 0 word words

hasWord :: Dictionary -> String -> Bool
hasWord (Dictionary words keys _) word = HashMap.member word words

toFile :: Dictionary -> String -> IO ()
toFile (Dictionary words _ _) filename = B.writeFile filename (encode words)
  
