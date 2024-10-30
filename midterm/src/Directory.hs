module Directory where

import Tree
import System.Directory (doesDirectoryExist, listDirectory)
import Control.Monad (forM)
import System.FilePath ((</>))
import qualified Data.Set as Set
import Data.List (sort)

-- Define an appropriate type called `DirectoryTree` to represent the directory hierarchy
type DirectoryTree = SetTree FilePath


-- Create a tree that represents the hierarchy of files and subdirectories of the given directory.
buildTree :: FilePath -> IO DirectoryTree
buildTree path = do
    isDir <- doesDirectoryExist path
    if isDir
        then do
            elements <- listDirectory path
            folders <- forM elements $ \name -> buildTree (path </> name)
            return $ Node path $ Set.fromList folders
        else return $ Leaf path

-- Implement a function that displays the subdirectories of the current directory before the files in it. 
defaultDisplayTree :: DirectoryTree -> String
defaultDisplayTree tree = display tree 0
  where
    display (Leaf path) spaces = replicate spaces ' ' ++ path ++ "\n"
    display (Node path subTrees) spaces =
        replicate spaces ' ' ++ path ++ "/\n" ++ concatMap (`display` (spaces + 2)) (Set.toList subTrees)

-- Implement a function that displays the subdirectories and the files in it in the alphabet order. 
alphabetisedDisplayTree :: DirectoryTree -> String
alphabetisedDisplayTree tree = display tree 0
  where
    display (Leaf path) spaces = replicate spaces ' ' ++ path ++ "\n"
    display (Node path subTrees) spaces =
        replicate spaces ' ' ++ path ++ "/\n" ++ concatMap (`display` (spaces + 2)) (sort (Set.toList subTrees))