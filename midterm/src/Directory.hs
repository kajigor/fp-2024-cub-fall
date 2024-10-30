module Directory where

import Tree
import qualified Data.Set as Set
import GHC.Exts (sortWith)

type DirectoryTree = SetTree String

getName (Leaf s) = s
getName (Node s _) = s

-- Create a tree that represents the hierarchy of files and subdirectories of the given directory.
buildTree :: FilePath -> IO DirectoryTree
buildTree = undefined

-- Implement a function that displays the subdirectories of the current directory before the files in it. 
defaultDisplayTree :: DirectoryTree -> String
defaultDisplayTree = displayWithOffset 0 where
    displayWithOffset n (Leaf s) = show (replicate n '\t') ++ s ++ "\n"
    displayWithOffset n (Node s children) = displayWithOffset n (Leaf s) ++ concatMap (displayWithOffset (n+1)) children

-- Implement a function that displays the subdirectories and the files in it in the alphabet order. 
alphabetisedDisplayTree :: DirectoryTree -> String
alphabetisedDisplayTree = displayWithOffset 0 where
    displayWithOffset n (Leaf s) = show (replicate n ' ') ++ s ++ "\n"
    displayWithOffset n (Node s children) = displayWithOffset n (Leaf s) ++ concatMap (displayWithOffset (n+1)) (sortWith getName $ Set.elems children)

main = putStrLn $ defaultDisplayTree (Leaf "abc")