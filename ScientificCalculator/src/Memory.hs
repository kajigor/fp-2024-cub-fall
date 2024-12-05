module Memory where

import Expr
import Control.Monad.State ( StateT, MonadState(get, put), modify )

-- newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }
type MemoryState m = StateT Expr m

initialState :: Expr
initialState = Num 0

memoryClear :: Monad m => MemoryState m ()
memoryClear = put (Num 0)

memoryPlus :: Monad m => Expr -> MemoryState m ()
memoryPlus expr = modify (`Add` expr)

memoryMinus :: Monad m => Expr -> MemoryState m ()
memoryMinus expr = modify (`Diff` expr)

memoryResult :: Monad m => MemoryState m Expr -- m (s, s)
memoryResult = get
               -- StateT $ \s -> return (s, s)