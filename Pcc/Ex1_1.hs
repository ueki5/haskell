module Pcc.Ex1_1 where
import Pcc.Comb
input = [382832,82823,38,38,3,3838,2238,389823,3888,388,3,3883,3882,904394,821,34828,3883,282332,3282,11,182,2,2,3834,38234,3824] :: [Int]
solve :: [Int] -> Int -> Bool
solve ns n = any (n ==)  (map sum $ comb ns 4)

-- ２分木
data Tree a = None
            | Leef a
            | Node (Tree a) a (Tree a)
            deriving (Show)
add :: (Ord a) => Tree a -> a -> Tree a
add None a = Leef a
add (Leef a1) a2 | a1 < a2  = Node  None     a1 (Leef a2)
                 | a1 >= a2 = Node (Leef a2) a1  None
add (Node left a1 right) a2 | a1 < a2  = Node   left          a1 (add right a2)
                            | a1 >= a2 = Node  (add left a2) a1  right
convTree :: (Ord a) => [a] -> Tree a
convTree cs = foldl add None cs
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (c:cs) = qsort left ++ [c] ++ qsort right
  where left  = [x|x <- cs,x <= c]
        right = [x|x <- cs,x >  c]