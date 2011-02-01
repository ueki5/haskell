module Ch18.Uwriter5 where
import Control.Monad
data Uist a = UistD a (Uist a) | Null deriving (Show)
instance Monad Uist where
  return a = UistD a Null
  (>>=) Null k = Null
  (>>=) (UistD a cs) k = (k a) `concate` (>>=) cs k
    where 
      concate :: Uist a -> Uist a -> Uist a
      Null `concate` as = as
      (UistD a as) `concate` as' = UistD a (as `concate` as')

ueki :: String -> Uist String
ueki [] = Null
ueki (c:cs) = UistD [c] $ UistD [c] $ ueki cs

uekiG :: String -> Uist String
uekiG s = ueki s 
          >>= ueki
-- data Ueki a = UekiD {execUeki::(a, String)} deriving (Show)
-- instance Monad Ueki where
--   return a = UekiD (a, [])
--   m >>= k = let (a, s) = execUeki m
--                 n = k a
--                 (b, s') = execUeki n
--             in UekiD (b, s' ++ s)
-- a :: [Int]
-- a = [0,1,2,3]
-- uekiI2S :: Int -> Ueki Int
-- uekiI2S cs = UekiD (cs, show cs)
