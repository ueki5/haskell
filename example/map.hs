map2                     :: (a->b) -> [a] -> [b]
map2 f  []               =  []
map2 f (x:xs)            =  f x : map2 f xs
