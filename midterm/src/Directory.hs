module Directory where

import Tree                       
import System.Directory (listDirectory)  
import System.FilePath ((</>))    
import qualified Data.Set as Set  
import Data.List (sort) 

-- Define an appropriate type called `DirectoryTree` to represent the directory hierarchy
newtype DirectoryTree = SetTree (SetTree FilePath)

-- Create a tree that represents the hierarchy of files and subdirectories of the given directory.
buildTree :: FilePath -> IO DirectoryTree
buildTree path = do
    contents <- listDirectory path
    if not (null contents)
        then do
            subDirects <- mapM (buildTree . (path </>)) contents
            return (SetTree (Node path (Set.fromList [tree | SetTree tree <- subDirects])))
        else
            return (SetTree (Leaf path))
    
helperDisplay :: SetTree FilePath -> Int -> String
helperDisplay (Leaf path) indent = replicate (2 * indent) ' ' ++ path ++ "\n"
helperDisplay (Node path children) indent =
    replicate (2 * indent) ' ' ++ path ++ "\n" ++
    concatMap (`helperDisplay` (indent + 1)) (sort children)

-- Implement a function that displays the subdirectories of the current directory before the files in it. 
defaultDisplayTree :: DirectoryTree -> String
defaultDisplayTree (SetTree tree) = helperDisplay tree 0

-- Implement a function that displays the subdirectories and the files in it in the alphabet order. 
alphabetisedDisplayTree :: DirectoryTree -> String
alphabetisedDisplayTree = undefined 