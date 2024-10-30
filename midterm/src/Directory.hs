module Directory where

import Tree()
import Data.Set (Set)
import qualified Data.Set as Set
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Data.List (sort)


-- Define an appropriate type called `DirectoryTree` to represent the directory hierarchy
data DirectoryTree
  = DirNode FilePath (Set DirectoryTree)
  | FileNode FilePath
  deriving (Show, Eq)

-- Create a tree that represents the hierarchy of files and subdirectories of the given directory.
buildTree :: FilePath -> IO DirectoryTree
buildTree path = do
    isDir <- doesDirectoryExist path
    if isDir
        then do
            contents <- listDirectory path
            subItems <- mapM (buildTree . (path </>)) contents
            let subTreeSet = Set.fromList subItems
            return (DirNode path subTreeSet)
        else
            return (FileNode path)


displayHelper :: Int -> DirectoryTree -> [String]
displayHelper indent (FileNode name) = [replicate (indent * 2) ' ' ++ name]
displayHelper indent (DirNode name subItems) =
    (replicate (indent * 2) ' ' ++ name) :
    concatMap (displayHelper (indent + 1)) sortedSubItems
  where
    sortedSubItems = sort (Set.toList subItems)

instance Ord DirectoryTree where
    compare (FileNode name1) (FileNode name2) = compare name1 name2
    compare (DirNode name1 _) (DirNode name2 _) = compare name1 name2
    compare (FileNode _) (DirNode _ _) = GT
    compare (DirNode _ _) (FileNode _) = LT


-- Implement a function that displays the subdirectories of the current directory before the files in it. 
defaultDisplayTree :: DirectoryTree -> String
defaultDisplayTree = unlines . displayHelper 0

-- Implement a function that displays the subdirectories and the files in it in the alphabet order. 
alphabetisedDisplayTree :: DirectoryTree -> String
alphabetisedDisplayTree = unlines . displayHelper 0
