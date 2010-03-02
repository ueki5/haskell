--add                     :: ( Integer , Integer ) ->  Integer
--add (x,y)                  =  x + y
add                     :: Integer -> Integer ->  Integer
add x y                  =  x + y
map2                     :: (a->b) -> [a] -> [b]
map2 f  []               =  []
map2 f (x:xs)           =  f x : map2 f xs
qsort []     = []
qsort (p:xs) = qsort lt ++ [p] ++ qsort gteq
                 where
                   lt   = [x | x <- xs, x < p]
                   gteq = [x | x <- xs, x >= p]
