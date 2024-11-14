module HW.Eval where

import qualified Data.Map as M
import State.MyState
import HW.StackMachine

-- Possible errors during evaluation
data Error v
  = StackUnderflow (StackInstr v) -- Not enough numbers on the stack to process the instruction
  | VarUndefined String           -- The variable is not defined in the environment 
  | StackNotExhausted Stack       -- After the program has finished evaluation, the stack is not a singleton
  | FinalStackEmpty
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
execInstr (PushNum n ) = do
  MachineState stack env <- get
  put $ MachineState (n : stack) env
  return (Right ())

execInstr (PushVar v ) = do
  MachineState stack env <- get
  case M.lookup v env of
    Just val -> do
      put $ MachineState (val : stack) env
      return $ Right ()
    Nothing -> return (Left (VarUndefined (show v)))

execInstr Add = do
  MachineState stack env <- get
  case stack of
    (x : y : rest) -> do
      put $ MachineState ((x + y) : rest) env
      return $ Right ()
    _ -> return (Left (StackUnderflow Add))

execInstr (StoreVar v ) = do
  MachineState stack env <- get
  case stack of
    (x : rest) -> do
      put $ MachineState rest (M.insert v x env)
      return $ Right ()
    _ -> return (Left (StackUnderflow (StoreVar v)))


-- Execute a list of instructions starting from the given state. 
execProgram :: (Ord v, Show v) => StackProgram v -> MachineState v -> Either (Error v) (MachineState v)
execProgram [] state =
  case getStack state of
    [ _ ] -> Right state
    []    -> Left FinalStackEmpty
    stack -> Left (StackNotExhausted stack)
execProgram (instr : instrs) state =
  let (newState, result) = runMyState (execInstr instr) state
   in case result of
        Left err -> Left err
        Right () -> execProgram instrs newState
