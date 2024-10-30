module Directory (DirectoryTree(..), buildTree, defaultDisplayTree, alphabetisedDisplayTree) where

import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Data.List (sort)

-- Define an appropriate type called `DirectoryTree` to represent the directory hierarchy
data DirectoryTree = File FilePath | Directory FilePath [DirectoryTree]
    deriving (Show, Eq)

-- Create a tree that represents the hierarchy of files and subdirectories of the given directory.
buildTree :: FilePath -> IO DirectoryTree
buildTree filePath = do
    check <- doesDirectoryExist filePath
    if check
        then do
            files <- listDirectory filePath
            let sortedFiles = sort files -- probably sorting by default is more efficient than separating by default(???)
            sub <- mapM (buildTree . (filePath </>)) sortedFiles
            return $ Directory filePath sub
        else
            return $ File filePath

-- Separate tree
partitionDirectoriesAndFiles :: [DirectoryTree] -> ([DirectoryTree], [DirectoryTree])
partitionDirectoriesAndFiles = foldr separate ([], [])
  where
    separate item (dirs, files) = case item of
        Directory _ _ -> (item : dirs, files)
        File _        -> (dirs, item : files)

-- Function to format the tree
formatTree :: String -> DirectoryTree -> String
formatTree indent (File filePath) = indent ++ "- " ++ filePath ++ "\n"
formatTree indent (Directory filePath files) =
    indent ++ "+ " ++ filePath ++ "\n" ++ concatMap (formatTree (indent ++ "  ")) files

-- Implement a function that displays the subdirectories of the current directory before the files in it. 
defaultDisplayTree :: DirectoryTree -> String
defaultDisplayTree (File filePath) = "- " ++ filePath ++ "\n"
defaultDisplayTree (Directory filePath files) =
    "+ " ++ filePath ++ "\n" ++ concatMap (formatTree "  ") (directories ++ files)
  where
    (directories, files) = partitionDirectoriesAndFiles files

-- Implement a function that displays the subdirectories and the files in it in the alphabet order. 
alphabetisedDisplayTree :: DirectoryTree -> String
alphabetisedDisplayTree = formatTree ""