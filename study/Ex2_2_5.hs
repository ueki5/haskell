module Pcc.Ex2_2_5 where
import Data.List

input = [1,3,5,8,2,5,9,24,3,2]::[Int]
solve :: [Int] -> [(Int,Int)]
solve ns = solve' [] (sort ns)
solve' :: [(Int,Int)] -> [Int] -> [(Int,Int)]
solve' ps [n] = ps
solve' ps ns = solve' (newpair:ps) nextlist
    where 
      first =  ns!!0
      second = ns!!1
      newpair = (first,second)
      newelem = first + second
      lesser  = [x| x <- drop 2 ns,x <  newelem]
      greater = [x| x <- drop 2 ns,x >= newelem]
      nextlist = lesser ++ [newelem] ++ greater
