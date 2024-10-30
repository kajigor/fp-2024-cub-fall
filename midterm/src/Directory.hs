module Directory where

import Tree (Tree(..))
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Data.List (sort)

type DirectoryTree = Tree FilePath

buildTree :: FilePath -> IO DirectoryTree
buildTree path = do
    isDir <- doesDirectoryExist path
    if isDir
       then do
           contents <- listDirectory path
           children <- mapM (buildTree . (path </>)) contents
           return $ Node path children
       else return $ Leaf path

defaultDisplayTree :: DirectoryTree -> String
defaultDisplayTree = unlines . display ""
  where
    display indent (Node dir children) =
        (indent ++ "+ " ++ dir) : concatMap (display (indent ++ "  ")) sortedChildren
      where
        (dirs, files) = splitDirsFiles children
        sortedChildren = dirs ++ files
    display indent (Leaf file) = [indent ++ "- " ++ file]

alphabetisedDisplayTree :: DirectoryTree -> String
alphabetisedDisplayTree = unlines . display ""
  where
    display indent (Node dir children) =
        (indent ++ "+ " ++ dir) : concatMap (display (indent ++ "  ")) (sortOnFileName children)
    display indent (Leaf file) = [indent ++ "- " ++ file]

splitDirsFiles :: [DirectoryTree] -> ([DirectoryTree], [DirectoryTree])
splitDirsFiles = foldr partition ([], [])
  where
    partition node@(Node _ _) (dirs, files) = (node : dirs, files)
    partition leaf@(Leaf _) (dirs, files) = (dirs, leaf : files)

sortOnFileName :: [DirectoryTree] -> [DirectoryTree]
sortOnFileName = sortOn getName
  where
    getName (Node name _) = name
    getName (Leaf name) = name

