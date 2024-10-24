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
  deriving (Show)

instance (Eq v) => Eq (MachineState v) where
   (MachineState s1 e1) == (MachineState s2 e2) = s1 == s2 && e1 == e2

-- Run the compiled program on an empty stack and environment
initialState :: MachineState String
initialState = MachineState [] M.empty

-- Execute a single instruction. 
-- Successful evaluation does not produce any useful result: only the effect of modifying state matters. 
execInstr :: (Ord v, Show v) => StackInstr v -> MyState (MachineState v) (Either (Error v) ())
execInstr (PushNum n) = do
   modify $ \state -> state {getStack = n : getStack state}
   return (Right ())
execInstr (PushVar v) = do
   state <- get
   case M.lookup v (getEnv state) of
     Just value -> do
        modify $ \state' -> state' {getStack = value : getStack state'}
        return (Right ())
     Nothing -> return (Left (VarUndefined (show v))) 
execInstr Add = do
   state <- get
   case getStack state of
     (x:y:more) -> do
        modify $ \state' -> state' {getStack = (x + y) : more}
        return (Right ())
     _ -> return (Left (StackUnderflow Add))
execInstr (StoreVar v) = do
    state <- get
    case getStack state of
      (x:more) -> do
         modify $ \state' -> state' {getStack = more, getEnv = M.insert v x (getEnv state')}
         return (Right())
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