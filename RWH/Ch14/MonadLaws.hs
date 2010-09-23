module MonadLaws where
import Control.Monad
data Ueki a = Ueki a
instance Monad Ueki where
    return x = Ueki x
--     (>>=) :: Ueki m => m a -> (a -> m b) -> m b
    (Ueki x) >>= f = f x
