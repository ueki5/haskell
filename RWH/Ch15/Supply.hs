{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ch15.Supply (
                    Supply
                   , next
                   , runSupply
                   ) where

import Control.Monad.State

-- newtype State s a = State {runState :: s -> (a, s)} -- Defined in Control.Monad.State.Lazy
-- instance Monad (State s) -- Defined in Control.Monad.State.Lazy
-- instance Functor (State s) -- Defined in Control.Monad.State.Lazy
-- instance MonadFix (State s) -- Defined in Control.Monad.State.Lazy
-- instance MonadState s (State s) -- Defined in Control.Monad.State.Lazy

newtype Supply s a = S (State [s] a)
-- unwrapS :: Supply s a -> State [s] a
-- unwrapS (S s) = s
-- instance Monad (Supply s) where
--     s >>= m = S (unwrapS s >>= unwrapS . m)
--     return = S . return
    deriving (Monad)
runSupply :: Supply s a -> [s] -> (a, [s])
runSupply (S m) xs = runState m xs
next :: Supply s (Maybe s)
next = S $ do
    st <- get
    case st of
      [] -> return Nothing
      (x:xs) -> do
                put xs
                return (Just x)


