module Ch11.Minimum where
import Ch11.QCbasics
import Test.QuickCheck
head' :: [a] -> a
head' [] = error "Prelude.head: empty list"
head' (c:_) = c
minimum' :: (Ord a) => [a] -> a
minimum' [] = error "Prelude.head': empty list"
minimum' [c] = c
minimum' (c:cs) = if c < min
                 then c
                 else min
    where min = minimum' cs
prop_minimum' xs = not (null xs) ==> head' (qsort xs) == minimum' xs

