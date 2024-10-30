module Directory where

import Tree
import qualified Data.Set as S
import Data.List (sort)
import System.Directory
import System.FilePath ((</>))

-- Define an appropriate type called `DirectoryTree` to represent the directory hierarchy
type DirectoryTree = SetTree FilePath


-- Create a tree that represents the hierarchy of files and subdirectories of the given directory.r

buildTree :: FilePath -> IO DirectoryTree
buildTree pathName = do
    isDir <- doesDirectoryExist pathName
    (if isDir then (do
        contents <- listDirectory pathName
        children <- mapM (buildTree . (pathName </>)) contents
        return $ Node pathName (S.fromList children)) else return $ Leaf pathName)


-- Implement a function that displays the subdirectories of the current directory before the files in it. 

defaultDisplayTree :: DirectoryTree -> String
defaultDisplayTree (Leaf pathName) = show pathName
defaultDisplayTree (Node pathName set) = concatMap ((\s -> "\t" ++ s ++ "\n") . defaultDisplayTree) $ S.toList set


-- Implement a function that displays the subdirectories and the files in it in the alphabet order. 
alphabetisedDisplayTree :: DirectoryTree -> String
alphabetisedDisplayTree (Leaf pathName) = show pathName
alphabetisedDisplayTree (Node pathName set) = concatMap ((\s -> "\t" ++ s ++ "\n") . defaultDisplayTree) $ sort $ S.toList set