module HW.Eval where

import qualified Data.Map          as M
import           Data.Map.Internal (lookup)
import           Data.Maybe        (fromJust, isJust)
import           HW.StackMachine
import           State.MyState
import           Util              (if', lengthLessThan)

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

getStackFromMachineState :: MachineState v -> Stack
getStackFromMachineState (MachineState getStack _) = getStack

getEnvFromMachineState :: MachineState v -> Env v
getEnvFromMachineState (MachineState _ getEnv) = getEnv

-- Run the compiled program on an empty stack and environment
initialState :: MachineState String
initialState = MachineState [] M.empty

-- Execute a single instruction.
-- Successful evaluation does not produce any useful result: only the effect of modifying state matters.
execInstr :: (Ord v, Show v) => StackInstr v -> MyState (MachineState v) (Either (Error v) ())
execInstr (PushNum x) = MyState run where
  run (MachineState stack env) =
    let stack' = x : stack in
      (MachineState stack' env, Right ())

execInstr (PushVar var) = MyState run where
  run (MachineState stack env) =
    let lookupResult = Data.Map.Internal.lookup var env in
      let stack' = if' (isJust lookupResult) (fromJust lookupResult : stack) stack in
        let err = if' (isJust lookupResult) (Right ()) $ Left (VarUndefined $ show var) in
          (MachineState stack' env, err)

execInstr Add = MyState run where
  run (MachineState stack env) =
    let failure = lengthLessThan stack 2 in
      let err = if' failure (Left $ StackUnderflow Add) (Right ()) in
        let stack' = if' failure stack ((head stack + head (tail stack)) : tail (tail stack)) in
          (MachineState stack' env, err)

execInstr (StoreVar var) = MyState run where
  run (MachineState stack env) =
    let failure = lengthLessThan stack 1 in
      let err = if' failure (Left $ StackUnderflow Add) (Right ()) in
        let stack' = if' failure stack (tail stack) in
          let env' = if' failure env (M.insert var (head stack) env) in
            (MachineState stack' env', err)

-- Execute a list of instructions starting from the given state.
execProgram :: (Ord v, Show v) => StackProgram v -> MachineState v -> Either (Error v) (MachineState v)
execProgram [] state@(MachineState [_] _) = Right state
execProgram [] (MachineState [] _) = Left FinalStackEmpty
execProgram [] (MachineState stack _) = Left (StackNotExhausted stack)
execProgram (i:is) state = let (state', possibleErr) = runMyState (execInstr i) state in
  case possibleErr of
    Left err -> Left err
    _        -> execProgram is state'
