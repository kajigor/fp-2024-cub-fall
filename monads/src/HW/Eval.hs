module HW.Eval (execInstr, execProgram, initialState) where

import qualified Data.Map as M
import State.MyState
import HW.StackMachine

-- Possible errors during evaluation
data Error v
  = StackUnderflow (StackInstr v)  -- Not enough numbers on the stack
  | VarUndefined String            -- Variable not defined in environment
  | StackNotExhausted Stack         -- Stack is not empty after evaluation
  deriving (Show, Eq)

type Stack = [Int]                  -- Stack holding integers
type Env v = M.Map v Int            -- Variable environment

data MachineState v = MachineState
  { getStack :: Stack
  , getEnv   :: Env v
  } deriving (Show)

-- Run the compiled program on an empty stack and environment
initialState :: MachineState String
initialState = MachineState [] M.empty

-- Execute a single instruction
execInstr :: (Ord v, Show v) => StackInstr v -> MyState (MachineState v) (Either (Error v) ())
execInstr (PushNum n) = modifyState $ \st -> Right $ st { getStack = n : getStack st }
execInstr (PushVar v) = modifyState $ \st ->
  case M.lookup v (getEnv st) of
    Just val -> Right $ st { getStack = val : getStack st }
    Nothing  -> Left $ VarUndefined (show v)
execInstr Add = modifyState $ \st ->
  case getStack st of
    (x:y:xs) -> Right $ st { getStack = (x + y) : xs }
    _        -> Left $ StackUnderflow Add
execInstr (StoreVar v) = modifyState $ \st ->
  case getStack st of
    (x:xs) -> Right $ st { getStack = xs, getEnv = M.insert v x (getEnv st) }
    _      -> Left $ StackUnderflow (StoreVar v)

-- Execute a list of instructions starting from the given state
execProgram :: (Ord v, Show v) => StackProgram v -> MachineState v -> Either (Error v) (MachineState v)
execProgram [] st = Right st
execProgram (instr:rest) st =
  case runMyState (execInstr instr) st of
    (Left err, _)         -> Left err
    (Right (), Right newState) -> execProgram rest newState

-- Modify the machine state
modifyState :: (MachineState v -> Either (Error v) (MachineState v)) -> MyState (MachineState v) (Either (Error v) ())
modifyState f = do
  st <- get
  case f st of
    Right newState -> put newState >> return (Right ())
    Left err       -> return (Left err)
