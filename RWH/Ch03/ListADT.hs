module ListADT where
data List a = Cons a (List a)
            | Nil
              deriving (Show,Ord,Eq)
fromList (x:xs) = Cons x (fromList xs)
fromList [] = Nil
toList :: List a -> [a]
toList Nil = []
toList (Cons x xs) = x:(toList xs)
a1 = Nil
a2 = Cons 1 a1
a3 = Cons 2 a2
l1 = []
l2 = 1:l1
l3 = 2:l2
b = (fromList l3) == a3