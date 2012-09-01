module Pcc.Ex2_2_4 where
input = [1,3,5,10,21,22,67,100,151,260]::[Int]

solve :: Int -> [Int] -> [Int]
solve r is = snd $ solve' r (is,[])
solve' :: Int -> ([Int],[Int]) -> ([Int],[Int])
solve' _ ([],os) = ([],os)
solve' r (is,os) = solve' r (rest,os ++ [checkpoint])
    where checkpoint = last $ takeWhile ((r + head is) >=) is
          rest = dropWhile ((r + head is) >=) is
           
                               
