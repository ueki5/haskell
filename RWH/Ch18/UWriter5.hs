module Ch18.Uwriter5 where
import Control.Monad
data Uist a = Null 
            | UistD a (Uist a) deriving (Show)
instance Monad Uist where
  return a = UistD a Null
  (>>=) Null k = Null
  (>>=) (UistD a cs) k = (k a) ++ (>>=) cs k
   where 
     (++) :: Uist a -> Uist a -> Uist a
     Null ++ as = as
     (UistD a as) ++ as' = UistD a (as ++ as')

ueki :: String -> String -> Uist String
ueki s [] = Null
ueki s (c:cs) = UistD s $ UistD [c] $ ueki s cs

uekiG :: String -> Uist String
uekiG s = ueki "a" s 
          >>= ueki "b"
uekiG' :: String -> Uist (String, String)
uekiG' s = do 
  x <- ueki "a" s 
  y <- ueki "b" x
  return (x,y)
uekiG'' :: String -> Uist (String,String)
uekiG'' s = ueki "a" s 
          >>= \x -> ueki "b" x
          >>= \y -> return (x, y)
