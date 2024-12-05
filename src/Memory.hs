module Memory (
    CalcState(..),
    initialState,
    storeMemory,
    clearMemory,
    clearAllMemory
) where

import qualified Data.Map as Map

-- calculator State
data CalcState = CalcState {
    memory :: Map.Map String Double
} deriving (Show)

initialState :: CalcState
initialState = CalcState Map.empty

storeMemory :: String -> Double -> CalcState -> CalcState
storeMemory key val state = 
    state { memory = Map.insert key val (memory state) }

clearMemory :: String -> CalcState -> CalcState
clearMemory key state = 
    state { memory = Map.delete key (memory state) }

clearAllMemory :: CalcState -> CalcState
clearAllMemory state = 
    state { memory = Map.empty }
