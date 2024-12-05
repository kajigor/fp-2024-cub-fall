{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Main (main) where

import Control.Monad.State
import Control.Monad (unless, when)
import Eval (eval)
import Expr (Expr(..))
import ParserExpr (parseInput, parseContinuation)
import Memory
import System.IO
import Data.Either
import Util

newtype CalculatorState = CalculatorState { lastResult :: Expr } -- Last successful evaluation

initialCalculatorState :: CalculatorState
initialCalculatorState = CalculatorState { lastResult = Num 0 }

handleCommand :: String -> StateT CalculatorState (MemoryState IO) ()
handleCommand input = case input of
  "ac" -> do
    put initialCalculatorState
    result <- gets lastResult
    case eval result of
      Left calcErr -> liftIO $ putStrLn $ "Error: " ++ show calcErr
      Right result -> liftIO $ putStrLn $ "Result: " ++ show result
  "m+" -> do
    result <- gets lastResult
    lift $ memoryPlus result
  "m-" -> do
    result <- gets lastResult
    lift $ memoryMinus result
  "mc" -> lift memoryClear >> liftIO (putStrLn "Memory cleared")
  "mr" -> do
    mem <- lift memoryResult
    liftIO $ putStrLn $ "Memory: " ++ show (fromRight  0 (eval mem))
  "=" -> do
    liftIO $ putStrLn "Enter an expression to evaluate:"
    exprInput <- liftIO getUserInput
    case parseInput exprInput of
      Left parseErr -> do 
        liftIO $ print parseErr
        liftIO $ print "Unknown command. Type 'help' for a list of commands."
      Right expr -> do
        case eval expr of
          Left calcErr -> liftIO $ putStrLn $ "Error: " ++ show calcErr
          Right result -> do
            liftIO $ putStrLn $ "Result: " ++ show result
            when (not (isNaN result) && not (isInfinite result)) $ do
              modify (\s -> s { lastResult = Num result })
  "help" -> liftIO $ putStrLn $ unlines helpString 
  input -> do
    lastResult <- gets lastResult
    let parsedExpr = parseContinuation lastResult input
    case parsedExpr of
      Left parseErr -> do
        liftIO $ print parseErr
        liftIO $ print "Unknown command. Type 'help' for a list of commands."
      Right expr -> do
        case eval expr of
          Left calcErr -> do 
            liftIO $ putStrLn $ "Error: " ++ show calcErr
          Right result -> do
            liftIO $ putStrLn $ "Result: " ++ show result
            when (not (isNaN result) && not (isInfinite result)) $ do
              modify (\s -> s { lastResult = Num result })

calculatorCLI :: StateT CalculatorState (MemoryState IO) ()
calculatorCLI = do
  liftIO $ putStr "> "
  liftIO $ hFlush stdout
  input <- liftIO getUserInput
  unless (input == "quit" || input == "q") $ do
    handleCommand $ removeTrailingSpaces input
    calculatorCLI


main :: IO ()
main = evalStateT (evalStateT calculatorCLI initialCalculatorState) initialState
                              -- StateT CalculatorState (MemoryState IO) ()
                              -- StateT CalculatorState (StateT Expr IO) ()
                  -- MemoryState IO ()
                  -- StateT Expr IO () 
       -- IO ()