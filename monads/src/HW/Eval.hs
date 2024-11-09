module HW.Eval where

import qualified Data.Map as M
import State.MyState
import HW.StackMachine

-- Possible errors during evaluation
data Error v
  = StackUnderflow (StackInstr v) -- Not enough numbers on the stack to process the instruction
  | VarUndefined String           -- The variable is not defined in the environment 
  | StackNotExhausted Stack       -- After the program has finished evaluation, the stack is not a singleton
  deriving (Show, Eq)

type Stack = [Int] -- The stack, holding integers

type Env v = M.Map v Int -- The environment, mapping variables to their values

-- The machine state consists of the stack and the variable environment.
data MachineState v = MachineState
  { getStack :: Stack,
    getEnv :: Env v
  }
  deriving (Show, Eq)

-- Run the compiled program on an empty stack and environment
initialState :: MachineState String
initialState = MachineState [] M.empty

-- Execute a single instruction. 
-- Successful evaluation does not produce any useful result: only the effect of modifying state matters. 
execInstr :: (Ord v, Show v) => StackInstr v -> MyState (MachineState v) (Either (Error v) ())
execInstr (PushNum n) = do
   modify $ \state -> state {getStack = n : getStack state}
execInstr (PushVar v) = do
   env <- gets getEnv
   case M.lookup v env of
     Just value -> do
        modify $ \state -> state {getStack = value : getStack state}
     Nothing -> return (Left (VarUndefined (show v))) 
execInstr Add = do
   state <- gets getStack
   case state of
     (x:y:more) -> do
        modify $ \state' -> state' {getStack = (x + y) : more}
     _ -> return (Left (StackUnderflow Add))
execInstr (StoreVar v) = do
    state <- gets getStack
    env <- gets getEnv
    case state of
      (x:more) -> do
         modify $ \state' -> state' {getStack = more, getEnv = M.insert v x env}
      _ -> return (Left(StackUnderflow (StoreVar v)))
 

-- Execute a list of instructions starting from the given state. 
execProgram :: (Ord v, Show v) => StackProgram v -> MachineState v -> Either (Error v) (MachineState v)
execProgram [] state =
   let finalState = getStack state
   in case finalState of
      [oneElement] -> Right state
      _ -> Left (StackNotExhausted finalState)
execProgram (x:moreInstr) state = do
   let result = execInstr x
   case runMyState result state of
     (_, Left err) -> Left err
     (tempState, Right ()) -> execProgram moreInstr tempState
