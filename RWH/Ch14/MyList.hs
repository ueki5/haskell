module Ch14.MyList where
data MyList a = MyCons a (MyList a)
              | Empty
                deriving (Show)
cons x xs = MyCons x xs
concate Empty ys = ys
concate xs Empty = xs
concate (MyCons x xs) ys = MyCons x (concate xs ys)

instance Monad MyList where
    Empty >>= k = Empty
    (MyCons x xs) >>= k = (k x) `concate` (xs >>= k)
    return x = MyCons x Empty
mylist2list :: MyList a -> MyList a -> MyList (a, a)
-- mylist2list xs ys = do
--   x <- xs
--   y <- ys
--   return (x, y)
mylist2list xs ys = xs >>= \x ->
                    ys >>= \y ->
                    return (x, y)
