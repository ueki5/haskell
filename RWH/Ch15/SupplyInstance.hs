{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ch15.SupplyInstance where
import Ch15.SupplyClass
import Ch15.RandomSupply

newtype Reader e a = R {runReader :: e -> a}
instance Monad (Reader e) where 
    return a = R $ \_ -> a
    m >>= k = R $ \r -> runReader (k (runReader m r)) r
--     m >>= k = R $ \r -> runReader (k (runReader m r)) r
--     m >>= k = R $ \r -> runReader (k (runReader m r)) r
ask :: Reader e e
ask = R id
newtype MySupply e a = MySupply { runMySupply :: Reader e a}
    deriving (Monad)

instance MonadSupply e (MySupply e) where
    next = MySupply $ do
             v <- ask
             return (Just v)

xy :: (Num s, MonadSupply s m) => m s
xy = do
  Just x <- next
  Just y <- next
  return (x * y)
runMS :: MySupply i a -> i -> a
runMS = runReader . runMySupply