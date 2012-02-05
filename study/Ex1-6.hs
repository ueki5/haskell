import List
input = [1,2,3,4,5,9,5,1,2]::[Int]
comb :: [Int] -> Int -> [[Int]]
-- comb ns n = [[x, y, z] | (x,ns' ) <- splits ns
--                        , (y,ns'') <- splits ns'
--                        , (z,_   ) <- splits ns'']
comb ns n = splits ns >>= \(x, ns' ) -> 
  splits ns' >>= \(y, ns'') -> 
  splits ns'' >>= \(z, ns''') -> 
  return [x, y, z]
comb' :: ([[Int]], [Int]) -> Int -> ([[Int]], [Int])
comb' ans 0 = ans
comb' (cmb, seed) n = (splits seed >>= 
                      \(x, seed') -> 
                      return (cmb ++ [x]) ,seed)
-- comb' :: [Int] -> [[Int]]
-- comb' ns = 
isPolygon :: [Int] -> Bool
isPolygon ns = isPolygon' $ reverse $ sort ns
isPolygon' (a:ns) =  a < (sum ns)
solve :: [Int] -> Int -> [Int]
solve i n = take 1 $ 
            reverse $ 
            sort $ 
            map sum $ 
            filter isPolygon $ 
            comb i n
splits :: [Int] -> [(Int, [Int])]
splits ns = [1 .. length ns] >>= \pos ->
  return $ splits' ns pos
splits' :: [Int] -> Int -> (Int,[Int])
splits' ns n = (head t,h ++ tail t)
  where
    h = take (n - 1) ns
    t = drop (n - 1) ns