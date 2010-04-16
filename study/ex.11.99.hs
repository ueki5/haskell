myfoldl :: (a -> b -> a) -> a -> [b] -> a
myfoldl f a [] = a
myfoldl f a (b:bs) = myfoldl f (f a b) bs

myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr f b [] = b
myfoldr f b (a:as) = f a (myfoldr f b as)