module Directory where

import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

import qualified Data.Set as Set
import Data.List (sortBy)
import Data.Function (on)

import Tree

-- Define an appropriate type called `DirectoryTree` to represent the directory hierarchy
type DirectoryTree = SetTree FilePath

-- Create a tree that represents the hierarchy of files and subdirectories of the given directory.
buildTree :: FilePath -> IO DirectoryTree
buildTree path = do
  exists <- doesDirectoryExist path
  if not exists
    then return (Leaf path)
    else do
      contents <- listDirectory path
      subtrees <- mapM (buildTree . (path </>)) contents
      let node = Node path (Set.fromList subtrees)
      return node

-- Implement a function that displays the subdirectories of the current directory before the files in it. 
defaultDisplayTree :: DirectoryTree -> String
defaultDisplayTree (Leaf a) = a ++ "\n"
defaultDisplayTree (Node a children) =
    a ++ "\n" ++ concatMap defaultDisplayTree (Set.toList children)

-- Implement a function that displays the subdirectories and the files in it in the alphabet order. 
alphabetisedDisplayTree :: DirectoryTree -> String
alphabetisedDisplayTree (Leaf a) = a ++ "\n"
alphabetisedDisplayTree (Node a children) =
    a ++ "\n" ++ concatMap alphabetisedDisplayTree sortedChildren
  where
    sortedChildren = sortBy (compare `on` getPath) (Set.toList children)
    getPath (Leaf path) = path
    getPath (Node path _) = path
