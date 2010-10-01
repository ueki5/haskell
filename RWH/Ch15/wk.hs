{-# LANGUAGE UndecidableInstances #-}
import Control.Monad
data Ueki a = UekiData a
            | UekiNothing
              deriving (Show)
instance Monad Ueki where
    return a = UekiData a
    UekiNothing >>= f = UekiNothing
    UekiData a >>= f = f a
instance MonadPlus Ueki where
    mzero = UekiNothing
    mplus UekiNothing y = y
    mplus (UekiData x) _ = UekiData x
uekiTest :: a -> Ueki ()
uekiTest x = mzero
-- Ueki2
-- data UekiInt Int = UekiDataInt Int
--               deriving (Show)
-- instance Monad UekiInt where
--     return a = UekiDataInt a
--     UekiDataInt a >>= f = f a
-- instance MonadPlus UekiInt where
--     mzero = UekiDataInt 0
--     mplus (UekiDataInt x) (UekiDataInt y) = UekiData (x + y)

-- uekiWork1 :: m ()

-- data Ueki a = Feki a
--             | UekiGo
-- mysum :: (Num a) => Ueki a -> Ueki a -> Ueki a
-- mysum (Feki x) (Feki y) = Feki (x + y)
-- mysum UekiGo y = y
-- mysum x UekiGo = x

-- instance Monad Ueki where
--     Feki a >>= f = f a
--     UekiGo >>= f = UekiGo
--     return a = Feki a

-- instance MonadPlus Ueki where
--     mzero = UekiGo
--     mplus = mysum

-- ueki :: Maybe Int -> Bool
-- ueki Nothing = False
-- ueki (Just n) | n < 0 = False
--               | otherwise = case odd n of
--                               True -> False 
--                               False -> True
