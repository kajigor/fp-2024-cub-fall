module Directory where

import qualified Data.Set as Set
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Tree

-- Define an appropriate type called `DirectoryTree` to represent the directory hierarchy
data DirectoryEntry = 
    File String
    | Directory String
    deriving (Show, Eq)

instance Ord DirectoryEntry where
    (File name1) `compare` (File name2) = name1 `compare` name2
    (Directory name1) `compare` (Directory name2) = name1 `compare` name2
    (Directory _) `compare` (File _) = LT
    (File _) `compare` (Directory _) = GT

type DirectoryTree = SetTree DirectoryEntry
-- Create a tree that represents the hierarchy of files and subdirectories of the given directory.
buildTree :: FilePath -> IO DirectoryTree
buildTree path = do
    isDir <- doesDirectoryExist path
    if isDir
        then do
            contents <- listDirectory path
            entries <- mapM buildEntry contents
            return $ Node (Directory (takeFileName path)) (Set.fromList entries)
        else
            error $ path ++ " is not a directory."
  where
    buildEntry :: FilePath -> IO (SetTree DirectoryEntry)
    buildEntry name = do
        let fullPath = path </> name
        isDir <- doesDirectoryExist fullPath
        if isDir
            then buildTree fullPath
            else return $ Leaf (File name)

    takeFileName :: FilePath -> String
    takeFileName = reverse . takeWhile (/= '/') . reverse

-- Implement a function that displays the subdirectories of the current directory before the files in it. 
defaultDisplayTree :: DirectoryTree -> String
defaultDisplayTree = undefined 

-- Implement a function that displays the subdirectories and the files in it in the alphabet order. 
alphabetisedDisplayTree :: DirectoryTree -> String
alphabetisedDisplayTree = undefined 