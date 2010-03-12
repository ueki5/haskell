smaller::Ord a=> a->[a]->[a]
smaller x xs = [y|y <- xs,y < x]
larger::Ord a=> a->[a]->[a]
larger x xs = [y|y <- xs,y >= x]
-- myquickosrt::Ord a => [a] -> [a]
myquicksort [] = []
myquicksort (x:xs) = myquicksort (smaller x xs) ++ [x] ++ myquicksort (larger x xs)
--                      where 
--                        smaller = [s|s <- xs,s <= x]
--                        larger = [l|l <- xs,x < l]