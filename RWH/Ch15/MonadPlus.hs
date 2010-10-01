{-# LANGUAGE TypeOperators #-}
import Control.Monad
----mzero :: (MonadPlus m) ==> m a
----(>>=) :: (Monad m) ==> m a -> (a -> m b) -> m b
--mzero >>= f == mzero
----(>>) :: (Monad m) ==> m a -> m b -> m b
--v >> mzero == mzero
--guard :: (MonadPlus m) => Bool -> m ()
-- guard True   =  return ()
-- guard False  =  mzero
-- zeroMod :: (Integral a,MonadPlus m) ==> a -> a -> m a
zeroMod :: (Integral a, MonadPlus m) => a -> a -> m a
x `zeroMod` n = guard ((x `mod` n) == 0) >> return x 
ckList :: [Int] -> [Int]
ckList ns = guard ((length ns) == 2) >> return (length ns)