module Directory where

import Tree
import qualified Data.Set as Set
import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath ((</>))
import Data.List (sortOn)
-- Define an appropriate type called `DirectoryTree` to represent the directory hierarchy
type DirectoryTree = SetTree FilePath

-- Create a tree that represents the hierarchy of files and subdirectories of the given directory.
buildTree :: FilePath -> IO DirectoryTree
buildTree path = do
    contents <- listDirectory path
    nodes <- mapM (buildNode path) contents
    return $ Node path (Set.fromList nodes)

buildNode :: FilePath -> FilePath -> IO (SetTree FilePath)
buildNode parent name = do
    let fullPath = parent </> name
    isDir <- doesDirectoryExist fullPath
    if isDir
      then buildTree fullPath
      else return (Leaf name)

-- Implement a function that displays the subdirectories of the current directory before the files in it. 
defaultDisplayTree :: DirectoryTree -> String
defaultDisplayTree = formatTree 0 id

-- Implement a function that displays the subdirectories and the files in it in the alphabet order. 
alphabetisedDisplayTree :: DirectoryTree -> String
alphabetisedDisplayTree = formatTree 0 (Set.fromList . sortOn getName . Set.toList)

formatTree :: Int -> (Set.Set DirectoryTree -> Set.Set DirectoryTree) -> DirectoryTree -> String
formatTree indent order (Leaf name) = replicate indent ' ' ++ name ++ "\n"
formatTree indent order (Node name children) =
    replicate indent ' ' ++ name ++ "\n" ++
    concatMap (formatTree (indent + 2) order) (Set.toList (order children))

getName :: DirectoryTree -> String
getName (Leaf name) = name
getName (Node name _) = name