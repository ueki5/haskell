input = [1,2,3,4,5,9,5,1,2]::[Int]
test :: [[a]] -> [a] -> [[a]]
test [] ns = ns >>= \x -> return [x]
test css ns = css >>= \cs -> 
               ns >>= \n ->
               return (cs ++ [n])
testn :: Int -> [[a]] -> [a] -> [[a]]
testn 0 css ns = css
testn n css ns = testn (n - 1) (test css ns) ns
splits :: [Int] -> [(Int, [Int])]
splits ns = [1 .. length ns] >>= \pos ->
  return $ splits' ns pos
splits' :: [Int] -> Int -> (Int,[Int])
splits' ns n = (head t,h ++ tail t)
  where
    h = take (n - 1) ns
    t = drop (n - 1) ns