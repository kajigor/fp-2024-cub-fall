module Directory where

import Tree

-- to Construct File Paths
import System.Directory (listDirectory, doesDirectoryExist)
-- other packages that I added
import System.FilePath ((</>))
import qualified Data.Set as Set
-- to sort alphabetically
import Data.List (sort)

-- Define an appropriate type called `DirectoryTree` to represent the directory hierarchy
-- newtype since only one constructor
newtype DirectoryTree = DirectoryTree (SetTree String)

-- Create a tree that represents the hierarchy of files and subdirectories of the given directory.
buildTree :: FilePath -> IO DirectoryTree
buildTree path = do
    exists <- doesDirectoryExist path
    if not exists
        then error "Bad path provided"
        else do
            contents <- listDirectory path
            children <- mapM (buildChild path) contents
            return $ DirectoryTree (Node path (Set.fromList children))

buildChild :: FilePath -> FilePath -> IO (SetTree String)
buildChild parent child = do
    let fullPath = parent </> child
    isDir <- doesDirectoryExist fullPath
    if isDir
        then do
            DirectoryTree subtree <- buildTree fullPath
            return subtree
        else return (Leaf fullPath)

-- Implement a function that displays the subdirectories of the current directory before the files in it. 
defaultDisplayTree :: DirectoryTree -> String
defaultDisplayTree (DirectoryTree tree) = displayTree tree ""
  where
    displayTree (Leaf name) indent = indent ++ name ++ "\n"
    displayTree (Node name children) indent =
        indent ++ name ++ "\n" ++ concatMap (`displayTree` (indent ++ "  ")) (Set.toList children)

-- Implement a function that displays the subdirectories and the files in it in the alphabet order. 
alphabetisedDisplayTree :: DirectoryTree -> String
alphabetisedDisplayTree (DirectoryTree tree) = displayTree tree
  where
    displayTree (Leaf name) = name ++ "\n"
    displayTree (Node name children) =
        name ++ "\n" ++ unlines (map (indent . displayTree) sortedChildren)
      where
        sortedChildren = sort (Set.toList children)
        indent child = "  " ++ child