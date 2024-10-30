module Directory where
import Tree
import System.Directory
import System.FilePath
import Data.List (sort, sortBy)
import Control.Monad (filterM)

data DirectoryTree = File FilePath
                  | Directory FilePath [DirectoryTree]
                  deriving (Show, Eq)

instance Ord DirectoryTree where
    compare (File path1) (File path2) = compare path1 path2
    compare (Directory path1 _) (Directory path2 _) = compare path1 path2
    compare (File path1) (Directory path2 _) = compare path1 path2
    compare (Directory path1 _) (File path2) = compare path1 path2

buildTree :: FilePath -> IO DirectoryTree
buildTree path = do
    isDir <- doesDirectoryExist path
    if isDir
        then do
            contents <- listDirectory path
            let fullPaths = map (path </>) contents
            realPaths <- filterM (fmap not . pathIsSymbolicLink) fullPaths
            subtrees <- mapM buildTree realPaths
            return $ Directory path subtrees
        else return $ File path

getBaseName :: FilePath -> String
getBaseName = takeBaseName . takeFileName
indent :: Int -> String
indent n = replicate (n * 2) ' '

displayTreeHelper :: Int -> DirectoryTree -> [String]
displayTreeHelper level (File path) = 
    [indent level ++ takeFileName path]
displayTreeHelper level (Directory path subdirs) = 
    (indent level ++ takeFileName path) : concatMap (displayTreeHelper (level + 1)) subdirs

sortByName :: [DirectoryTree] -> [DirectoryTree]
sortByName = sortBy compareNames
  where
    compareNames t1 t2 = compare (getName t1) (getName t2)
    getName (File p) = takeFileName p
    getName (Directory p _) = takeFileName p

defaultDisplayTree :: DirectoryTree -> String
defaultDisplayTree tree = unlines $ displayTreeHelper 0 $
    sortTreeByType tree

sortTreeByType :: DirectoryTree -> DirectoryTree
sortTreeByType (File path) = File path
sortTreeByType (Directory path items) =
    Directory path $ 
        let (dirs, files) = partition isDirectory items
            sortedDirs = map sortTreeByType $ sortByName dirs
            sortedFiles = sortByName files
        in sortedDirs ++ sortedFiles
  where
    isDirectory (Directory _ _) = True
    isDirectory _ = False

alphabetDisplayTree :: DirectoryTree -> String
alphabetDisplayTree tree = unlines $ displayTreeHelper 0 $
    sortTreeAlphabetically tree
sortTreeAlphabetically :: DirectoryTree -> DirectoryTree
sortTreeAlphabetically (File path) = File path
sortTreeAlphabetically (Directory path items) =
    Directory path $ 
        sortByName $ map sortTreeAlphabetically items

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p = foldr select ([], [])
  where
    select x (ts, fs) | p x = (x:ts, fs)
                      | otherwise = (ts, x:fs)