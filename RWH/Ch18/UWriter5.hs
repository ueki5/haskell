module Ch18.Uwriter5 where
import Control.Monad
data Uist a = Null 
            | UistD a (Uist a) 
            deriving (Show)
append :: Uist a -> Uist a -> Uist a
Null `append` as = as
(UistD a as) `append` as' = UistD a (as `append` as')
instance Monad Uist where
  return a = UistD a Null
  (>>=) Null k = Null
  (>>=) (UistD a cs) k = (k a) `append` (>>=) cs k

k1 :: String -> String -> Uist String
k1 s [] = Null
k1 s (c:cs) = UistD s $ UistD [c] $ k1 s cs

test1 :: String -> Uist String
test1 s = k1 "a" s 
          >>= k1 "b"
test1' :: String -> Uist (String, String)
test1' s = do 
  x <- k1 "a" s 
  y <- k1 "b" x
  return (x,y)
test1'' :: String -> Uist (String,String)
test1'' s = k1 "a" s 
          >>= \x -> k1 "b" x
          >>= \y -> return (x, y)
forU :: (Monad m) => Uist a -> (a -> m b) -> m (Uist b)
forU Null k = return Null
forU (UistD c cs) k = k c >>= 
                      \b -> forU cs k >>= 
                      \bs -> return (UistD b bs)
mapU :: (Monad m) => (a -> m b) -> Uist a  -> m (Uist b)
mapU k m = forU m k
aint = UistD 4 $ UistD 3 $ UistD 2 $ UistD 1 Null :: Uist Int
data Mona1 a = MonaD1 {k2d::a} deriving (Show)
instance Monad Mona1 where
  return a = MonaD1 a
  m >>= k = k $ k2d m
k2 :: Int -> Mona1 Int
k2 n = MonaD1 $ n * 2
test2 :: Uist Int -> Mona1 (Uist Int)
test2 ns = forU aint k2
uoldr :: (a -> b -> b) -> b -> Uist a -> b
uoldr k b Null = b 
uoldr k b (UistD a as) = k a (uoldr k b as)
uoldl :: (a -> b -> a) -> a -> Uist b -> a
uoldl k a Null = a
uoldl k a (UistD b bs) = uoldl k (k a b) bs
k3 :: Int -> Int -> Int
k3 a b = a `div` b
test3 :: Int
test3 = uoldr k3 1 aint
test4 :: Int
test4 = uoldl k3 1 aint
liftU ::(Monad m) => m a -> (a -> b) -> m b
liftU m k = m >>= 
            \a -> return $ k a
test5 :: Uist Int -> Uist Int
test5 as = liftU as (*2)