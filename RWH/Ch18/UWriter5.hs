-- uekic5 = forM [0,1,2,3] \x-> x**2
import Control.Monad

data Ueki a = UekiD {execUeki::(a, String)} deriving (Show)
instance Monad Ueki where
  return a = UekiD (a, [])
  m >>= k = let (a, s) = execUeki m
                n = k a
                (b, s') = execUeki n
            in UekiD (b, s' ++ s)
a :: [Int]
a = [0,1,2,3]
ueki5 :: Int -> Ueki Int
ueki5 cs = UekiD (cs, show cs)
-- myForM :: (Monad m) => [a] -> (a -> m b) -> m [b]
-- myForM [] k = 
  