module Main (main) where

import System.Console.Terminal.Size (size, Window (width))
import qualified Parser
import qualified Dictionary
import Dictionary (Dictionary)
import Lib
import Formatter


check :: Dictionary -> String -> IO ()
check dict file = do
  Just windowSize <- size
  let windowWidth = width windowSize
  content <- readFile file
  let wrapped = wrap windowWidth content
  let errors = spellcheck dict wrapped
  putStrLn $ highlightErrors wrapped errors

addWord :: Dictionary -> String -> String -> IO ()
addWord dict dictFilename word = 
  if Dictionary.hasWord dict word
  then putStrLn "Word already exists"
  else do
    let dict' = Dictionary.addWord dict word
    Dictionary.toFile dict' dictFilename

removeWord :: Dictionary -> String -> String -> IO ()
removeWord dict dictFilename word = if not (Dictionary.hasWord dict word)
  then putStrLn "Word doesn't exist"
  else do
    let dict' = Dictionary.removeWord dict word
    Dictionary.toFile dict' dictFilename

main :: IO ()
main = do
  opts <- Parser.getOptions
  let dictFilename = Parser.getDictionaryFilename opts
  dict <- Dictionary.fromFile dictFilename
  let cmd = Parser.getCommand opts
  case cmd of
    Parser.Check file -> check dict file
    Parser.AddWord word -> addWord dict dictFilename word
    Parser.RemoveWord word -> removeWord dict dictFilename word
