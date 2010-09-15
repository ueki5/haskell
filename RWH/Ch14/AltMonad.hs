module Ch14.AltMonad where
import Prelude hiding ((>>=), return)

class Functor m => AltMonad m where
    join :: m (m a) -> m a
    return :: a -> m a
    (>>=) ::  m a -> (a -> m b) -> m b
    xs >>= f = join (fmap f xs)
    liftM :: (a -> b) -> m a -> m b
    liftM f x = x >>= (return . f)
