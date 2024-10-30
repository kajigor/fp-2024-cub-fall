module Directory where

import Tree
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeFileName)
import Data.List (sort)
import qualified Data.Set as Set

-- Define an appropriate type called `DirectoryTree` to represent the directory hierarchy
data DirectoryTree
  = Dir FilePath (Set.Set DirectoryTree)
  | File FilePath                        
  deriving (Show, Eq, Ord)

-- Create a tree that represents the hierarchy of files and subdirectories of the given directory.
buildTree :: FilePath -> IO DirectoryTree
buildTree path = do
  isDir <- doesDirectoryExist path
  if isDir
    then do
      contents <- listDirectory path
      children <- mapM (buildTree . (path </>)) contents
      let sortedChildren = Set.fromList children
      return $ Dir (takeFileName path) sortedChildren
    else return $ File (takeFileName path) 

-- Implement a function that displays the subdirectories of the current directory before the files in it. 
defaultDisplayTree :: DirectoryTree -> String
defaultDisplayTree = displayTree False 0 

-- Implement a function that displays the subdirectories and the files in it in the alphabet order. 
alphabetisedDisplayTree :: DirectoryTree -> String
alphabetisedDisplayTree = displayTree True 0

displayTree :: Bool -> Int -> DirectoryTree -> String
displayTree alphabetic indent (Dir path contents) =
  replicate indent ' ' ++ path ++ "\n" ++
  concatMap (displayTree alphabetic (indent + 2)) sortedContents
  where
    sortedContents = if alphabetic
                     then sort $ Set.toList contents
                     else Set.toList contents
displayTree _ indent (File path) =
  replicate indent ' ' ++ path ++ "\n"
