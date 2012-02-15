module Pcc.Ex1_6 where
import List
import Pcc.Comb

-- input = [1,5,3,9]::[Int]
input = [1,2,3,4,5,9,5,1,2]::[Int]
-- 問題
isPolygon :: [Int] -> Bool
isPolygon ns = isPolygon' $ reverse $ sort ns
isPolygon' [] = False
isPolygon' (longest:rest) =  longest < (sum rest)
solve :: [Int] -> Int -> [Int]
solve input n = take 1 $ 
            reverse $ 
            sort $ 
            map sum $ 
            filter isPolygon $ 
            comb input n
