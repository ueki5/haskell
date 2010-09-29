{-# LANGUAGE UndecidableInstances #-}
import Control.Monad

data Ueki a = Feki a
            | UekiGo
mysum :: (Num a) => Ueki a -> Ueki a -> Ueki a
mysum (Feki x) (Feki y) = Feki (x + y)
mysum UekiGo y = y
mysum x UekiGo = x

instance Monad Ueki where
    Feki a >>= f = f a
    UekiGo >>= f = UekiGo
    return a = Feki a

instance MonadPlus Ueki where
    mzero = UekiGo
    mplus = mysum
