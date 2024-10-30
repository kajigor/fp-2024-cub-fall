module Directory where

import Tree
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import qualified Data.Set as Set

type DirectoryTree = SetTree FilePath

-- Create a tree that represents the hierarchy of files and subdirectories of the given directory.
buildTree :: FilePath -> IO DirectoryTree
buildTree files = do
    exist <- doesDirectoryExist files
    if exist then do 
        directoryContent <- listDirectory files
        subtr <- mapM (buildTree . (files </>)) directoryContent
        return $ Node files (Set.fromList subtr)
    else return $ Leaf files

-- Implement a function that displays the subdirectories of the current directory before the files in it. 
defaultDisplayTree :: DirectoryTree -> String
defaultDisplayTree (Leaf file) = file ++ "\n"
defaultDisplayTree (Node dir subdirs) = dir ++ "\n" ++
                                        let (d, f) = Set.partition isNode subdirs
                                            sortedFiles = Set.toList d ++ Set.toList f
                                        in concat [helper (file) (indent ++ " ") ]

-- Implement a function that displays the subdirectories and the files in it in the alphabet order. 
alphabetisedDisplayTree :: DirectoryTree -> String
alphabetisedDisplayTree = undefined 