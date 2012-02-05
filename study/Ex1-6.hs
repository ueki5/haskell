import List
input = [1,2,3,4,5,9,5,1,2]::[Int]
comb :: [Int] -> Int -> [[Int]]
comb as n = [[x, y, z] | (x,as' ) <- splits as
                       , (y,as'') <- splits as'
                       , (z,_   ) <- splits as'']
isPolygon :: [Int] -> Bool
isPolygon as = isPolygon' $ reverse $ sort as
isPolygon' (a:as) =  a < (sum as)
solve :: [Int] -> Int -> [Int]
solve i n = take 1 $ 
            reverse $ 
            sort $ 
            map sum $ 
            filter isPolygon $ 
            comb i n
splits :: [Int] -> [(Int, [Int])]
splits ns = [1 .. length ns] >>= \pos ->
  return $ split' ns pos
split' :: [Int] -> Int -> (Int,[Int])
split' ns n = (head t,h ++ tail t)
  where
    h = take (n - 1) ns
    t = drop (n - 1) ns