{-# LANGUAGE TupleSections#-}
module Ch14.MultiplyTo where
guarded :: Bool -> [a] -> [a]
guarded True xs = xs
guarded False _ = []

multiplyTo :: Int -> [(Int, Int)]
-- multiplyTo n = do
--   x <- [1 .. n]
--   y <- [x .. n]
--   return (x, y)
-- multiplyTo n =  [1 .. n] >>= \x ->
--                 map (x,) [x .. n]
multiplyTo n = do
  x <- [1 .. n]
  y <- [x .. n]
--   guarded (x * y == n) $ return (x, y)
  if x * y == n 
       then return (x, y) 
       else []
myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ [] = []
myfilter f (x:xs) 
         | f x = x : myfilter f xs
         | otherwise = myfilter f xs

myTest xs ys = xs >>~ \x ->
               ys >>~ \y ->
               if odd (x * y)
                   then return (x,y)
                   else fail ""
(>>~) :: [a] -> (a -> [b]) -> [[b]]
xs >>~ f = map f xs