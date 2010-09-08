{-# LANGUAGE TupleSections #-}
module Ch14.MyList where
import Control.Monad
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
instance Functor MyList where
    fmap f Empty = Empty
    fmap f (MyCons x xs) = MyCons (f x) (fmap f xs)
-- mylist2list :: MyList a -> MyList a -> MyList (a, a)
-- mylist2list xs ys = do
--   x <- xs
--   y <- ys
--   return (x, y)
-- mylist2list xs ys = xs >>= \x ->
--                     ys >>= \y ->
--                     return (x, y)
mylist2list Empty ys = Empty
mylist2list xs Empty = Empty
mylist2list (MyCons x xs) ys =
    return (fmap (x,) ys) `concate` (mylist2list xs ys)
mylist2list' xs ys = xs >>
                     ys >>
                     return Empty
list1 = (MyCons   2 (MyCons   1 (MyCons   0 Empty)))
list2 = (MyCons 102 (MyCons 101 (MyCons 100 Empty)))