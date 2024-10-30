module Directory where

import Tree
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import qualified Data.Set as S

-- Define an appropriate type called `DirectoryTree` to represent the directory hierarchy
type DirectoryTree = SetTree FilePath

-- Create a tree that represents the hierarchy of files and subdirectories of the given directory.
buildTree :: FilePath -> IO DirectoryTree
buildTree path = do
    dir <- doesDirectoryExist path
    if dir then do
        dirContent <- listDirectory path
        subTrees <- mapM (buildTree . (path </>)) dirContent
        let leaves = S.fromList subTrees
        return $ Node path leaves
    else return $ Leaf path

-- Implement a function that displays the subdirectories of the current directory before the files in it. 
defaultDisplayTree :: DirectoryTree -> String
defaultDisplayTree = helper 0
  where
    helper :: Int -> DirectoryTree -> String
    helper indent (Leaf path) =
        replicate (indent * 2) ' ' ++ path ++ "\n"
    helper indent (Node path subTrees) =
        replicate (indent * 2) ' ' ++ path ++ "/\n" ++
        concatMap (helper (indent + 1)) (S.toList subTrees)

-- Implement a function that displays the subdirectories and the files in it in the alphabet order. 
alphabetisedDisplayTree :: DirectoryTree -> String
alphabetisedDisplayTree = sndHelper 0
  where
    sndHelper :: Int -> DirectoryTree -> String
    sndHelper indent (Leaf path) =
        replicate (indent * 2) ' ' ++ path ++ "\n"
    sndHelper indent (Node path subTrees) =
        replicate (indent * 2) ' ' ++ path ++ "/\n" ++
        concatMap (sndHelper (indent + 1)) (sortedSubTrees subTrees)

    sortedSubTrees :: S.Set (SetTree FilePath) -> [DirectoryTree]
    sortedSubTrees = S.toList . S.fromList . S.toList