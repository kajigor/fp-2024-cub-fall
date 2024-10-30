module Directory where

import Tree

import qualified Data.Set as Set
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Control.Monad (forM)
-- Define an appropriate type called `DirectoryTree` to represent the directory hierarchy

data DirectoryTree
    = Path FilePath
    | Directory FilePath [DirectoryTree]

-- Create a tree that represents the hierarchy of files and subdirectories of the given directory.
buildTree :: FilePath -> IO DirectoryTree
buildTree path = do
    isDir <- doesDirectoryExist path
    if isDir
        then do
            contents <- listDirectory path
            items <- forM contents $ \name -> buildTree (path </> name)
            return $ Directory path items
        else return $ Path path


-- Implement a function that displays the subdirectories of the current directory before the files in it. 
defaultDisplayTree :: DirectoryTree -> String
defaultDisplayTree = outp ""
  where
    outp pr (Path name) = pr ++ name ++ "\n"
    outp pr (Directory name contents) =
        pr ++ name ++ "/\n" ++ concatMap (outp (pr ++ "    ")) contents

-- Implement a function that displays the subdirectories and the files in it in the alphabet order. 
alphabetisedDisplayTree :: DirectoryTree -> String
alphabetisedDisplayTree = undefined 