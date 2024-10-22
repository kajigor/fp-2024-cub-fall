module HW.Eval where

import qualified Data.Map as M
import HW.StackMachine
import State.MyState

-- Possible errors during evaluation
data Error v
  = StackUnderflow (StackInstr v) -- Not enough numbers on the stack to process the instruction
  | VarUndefined String -- The variable is not defined in the environment
  | StackNotExhausted Stack -- After the program has finished evaluation, the stack is not a singleton
  deriving (Show, Eq)

type Stack = [Int] -- The stack, holding integers

type Env v = M.Map v Int -- The environment, mapping variables to their values

-- The machine state consists of the stack and the variable environment.
data MachineState v = MachineState
  { getStack :: Stack,
    getEnv :: Env v
  }
  deriving (Show)

-- Run the compiled program on an empty stack and environment
initialState :: MachineState String
initialState = MachineState [] M.empty

-- Execute a single instruction.
-- Successful evaluation does not produce any useful result: only the effect of modifying state matters.
execInstr :: (Ord v, Show v) => StackInstr v -> MyState (MachineState v) (Either (Error v) ())
execInstr (PushNum x) = do
  MachineState s env <- get
  put (MachineState (x : s) env)
  return (Right ())
execInstr (PushVar x) = do
  MachineState s env <- get
  case M.lookup x env of
    Just val -> do
      put (MachineState (val : s) env)
      return (Right ())
    Nothing -> return (Left (VarUndefined (show x)))
execInstr Add = do
  MachineState s env <- get
  case s of
    (x : y : rest) -> do
      let result = x + y
      put (MachineState (result : rest) env)
      return (Right ())
    _ -> return (Left (StackUnderflow Add))
execInstr (StoreVar v) = do
  MachineState s env <- get
  case s of
    (val : rest) -> do
      put (MachineState rest (M.insert v val env))
      return (Right ())
    _ -> return (Left (StackUnderflow (StoreVar v)))

-- Execute a list of instructions starting from the given state.
execProgram :: (Ord v, Show v) => StackProgram v -> MachineState v -> Either (Error v) (MachineState v)
execProgram [] state = Right state
execProgram (instruction : instructions) state = do
  case runMyState (execInstr instruction) state of
    (newState, Left err) -> Left err
    (newState, Right ()) -> execProgram instructions newState