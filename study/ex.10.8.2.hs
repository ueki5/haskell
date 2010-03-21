data List a = Nil | Cons a (List a)
len :: List a -> Int
len Nil = 0
len (Cons x xs) = 1 + (len xs)
cons :: a -> List a -> List a
cons Nil xs = xs
cons 