module HW.Eval where

import qualified Data.Map          as M
import           Data.Map.Internal (lookup)
import           HW.StackMachine
import           State.MyState

-- Possible errors during evaluation
data Error v
  = StackUnderflow (StackInstr v) -- Not enough numbers on the stack to process the instruction
  | VarUndefined String           -- The variable is not defined in the environment
  | StackNotExhausted Stack       -- After the program has finished evaluation, the stack is not a singleton
  | FinalStackEmpty               -- After the program has finished evaluation, the stack is empty
  deriving (Show, Eq)

type Stack = [Int] -- The stack, holding integers

type Env v = M.Map v Int -- The environment, mapping variables to their values

-- The machine state consists of the stack and the variable environment.
data MachineState v = MachineState
  { getStack :: Stack,
    getEnv   :: Env v
  }
  deriving (Show, Eq)

-- Run the compiled program on an empty stack and environment
initialState :: MachineState String
initialState = MachineState [] M.empty

-- Execute a single instruction.
-- Successful evaluation does not produce any useful result: only the effect of modifying state matters.
execInstr :: (Ord v, Show v) => StackInstr v -> MyState (MachineState v) (Either (Error v) ())
execInstr (PushNum x) = Right <$> modify (\mstate -> mstate { getStack = x : getStack mstate })

execInstr (PushVar var) = do
  MachineState stack env <- get
  case Data.Map.Internal.lookup var env of
    Just x -> Right <$> modify (\mstate -> mstate { getStack = x : stack })
    Nothing -> return (Left $ VarUndefined $ show var)

execInstr Add = do
  MachineState stack _ <- get
  case stack of
    a:b:cs -> Right <$> modify (\mstate -> mstate { getStack = (a+b):cs })
    _ -> return (Left $ StackUnderflow Add)

execInstr instr@(StoreVar var) = do
  MachineState stack env <- get
  case stack of
    a:as -> Right <$> modify (const $ MachineState as (M.insert var a env))
    _ -> return (Left $ StackUnderflow instr)

-- Execute a list of instructions starting from the given state.
execProgram :: (Ord v, Show v) => StackProgram v -> MachineState v -> Either (Error v) (MachineState v)
execProgram [] state@(MachineState [_] _) = Right state
execProgram [] (MachineState [] _) = Left FinalStackEmpty
execProgram [] (MachineState stack _) = Left (StackNotExhausted stack)
execProgram (i:is) state = let (state', possibleErr) = runMyState (execInstr i) state in
  case possibleErr of
    Left err -> Left err
    _        -> execProgram is state'
