module Directory where

import Tree
import Control.Monad (forM)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeFileName)
import qualified Data.Set as Set 
import Data.List (sort)

-- Define an appropriate type called `DirectoryTree` to represent the directory hierarchy
type DirectoryTree = SetTree FilePath

-- Create a tree that represents the hierarchy of files and subdirectories of the given directory.
buildTree :: FilePath -> IO DirectoryTree
buildTree path = do
  isDir <- doesDirectoryExist path
  if not isDir
    then return (Leaf path)
    else do
      contents <- listDirectory path
      subTrees <- forM contents $ \name -> buildTree (path </> name)
      let sortedSubTrees = Set.fromList subTrees
      return (Node path sortedSubTrees)

-- Implement a function that displays the subdirectories of the current directory before the files in it. 
defaultDisplayTree :: DirectoryTree -> String
defaultDisplayTree (Leaf path) = takeFileName path ++ "\n"
defaultDisplayTree (Node path subTrees) =
  let header = takeFileName path ++ "\n" 
      subTreeStrings = concatMap formatSubTree (Set.toList subTrees)
  in header ++ subTreeStrings
  where
    formatSubTree :: DirectoryTree -> String
    formatSubTree (Leaf file) = "  " ++ takeFileName file ++ "\n"  
    formatSubTree (Node subDir subChildren) =
      "  " ++ takeFileName subDir ++ "\n" ++ defaultDisplayTree (Node subDir subChildren)

-- Implement a function that displays the subdirectories and the files in it in the alphabet order. 
alphabetisedDisplayTree :: DirectoryTree -> String
alphabetisedDisplayTree (Leaf path) = takeFileName path ++ "\n"
alphabetisedDisplayTree (Node path subTrees) =
  let header = takeFileName path ++ "\n" 
      sortedSubTrees = sort (Set.toList subTrees)  
      subTreeStrings = concatMap formatSubTree sortedSubTrees
  in header ++ subTreeStrings
  where
    formatSubTree :: DirectoryTree -> String
    formatSubTree (Leaf file) = "  " ++ takeFileName file ++ "\n"  
    formatSubTree (Node subDir subChildren) =
      "  " ++ takeFileName subDir ++ "\n" ++ alphabetisedDisplayTree (Node subDir subChildren) 
