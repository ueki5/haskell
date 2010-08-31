module Ch11.QCbasics where 
import Test.QuickCheck
import Data.List

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (c:cs) = qsort lesser ++ (c:qsort larger)
    where lesser = filter (  <  c) cs
          larger = filter (c <=  ) cs
prop_idempotest xs = qsort (qsort xs) == qsort xs
prop_minimum xs = head (qsort xs) == minimum xs
test fnc = quickCheck (fnc :: [Integer] -> Bool)
prop_ordered xs = ordered (qsort xs)
    where ordered [] = True
          ordered [x] = True
          ordered (x:y:xs) = x <= y && ordered (y:xs)

prop_permutation xs = permutation xs (qsort xs)
    where permutation xs ys = null (xs \\ ys) && null (ys \\ xs)

prop_maximum xs =
    not (null xs) ==>
        last (qsort xs) == maximum xs

prop_append xs ys =
    not (null xs) ==>
    not (null ys) ==>
        head (qsort (xs ++ ys)) == min (minimum xs) (minimum ys)

prop_sort_model xs = sort xs == qsort xs